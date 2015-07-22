%% -------------------------------------------------------------------
%% conf_dyn - dynamic config generate
%%
%% Copyright (c) 2014-2015 Shion Ryuu (shionryuu@outlook.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(conf_gen).
-author("Ryuu").

-include("conf_dyn.hrl").

%% API
-export([
    parallel_generate/1,
    generate_file/1
]).

%% 
-export([
    output_source/3
]).

%% @doc 并行生成
parallel_generate(Maps) ->
    MaxWorker = erlang:system_info(logical_processors_available),
    JobList = do_split_list(Maps, MaxWorker),
    %% 启动进程
    Ref = make_ref(),
    Pids =
        [begin
             start_worker(Jobs, self(), Ref)
         end || Jobs <- JobList, Jobs =/= []],
    do_wait_worker(length(Pids), Ref).

%% @doc 启动worker进程
start_worker(Jobs, Parent, Ref) ->
    spawn_link(fun() ->
        [begin
             case generate_file(Map) of
                 {error, Error} ->
                     Parent ! {error, Error, Ref},
                     exit(error);
                 _ ->
                     ok
             end
         end || Map <- Jobs],
        Parent ! {ack, Ref}
    end).

%% @doc 等待结果, adapted from mmake
do_wait_worker(0, _Ref) ->
    ok;
do_wait_worker(N, Ref) ->
    receive
        {ack, Ref} ->
            do_wait_worker(N - 1, Ref);
        {error, Error, Ref} ->
            throw({error, Error});
        {'EXIT', _P, _Reason} ->
            do_wait_worker(N, Ref);
        _Other ->
            error_logger:info_msg("receive unknown msg:~p~n", [_Other]),
            do_wait_worker(N, Ref)
    end.

%% @doc 将L分割成最多包含N个子列表的列表
do_split_list(L, N) ->
    Len = length(L),
    % 每个列表的元素数
    LLen = (Len + N - 1) div N,
    do_split_list(L, LLen, []).

do_split_list([], _N, Acc) ->
    lists:reverse(Acc);
do_split_list(L, N, Acc) ->
    {L2, L3} = lists:split(erlang:min(length(L), N), L),
    do_split_list(L3, N, [L2 | Acc]).

%% @doc 根据映射生成配置
generate_file(Map) ->
    #conf_map{name = ConfName, path = RelPath, type = MapType} = Map,
    case file:path_consult([conf_util:get_config_dir()], RelPath) of
        {ok, TermList, _FullName} ->
            error_logger:info_msg("generate config ~w succeed~n", [ConfName]),
            output_file(TermList, ConfName, MapType);
        {error, Error} ->
            error_logger:info_msg("generate config ~w failed: ~w~n", [ConfName, Error]),
            {error, Error}
    end.

%% @doc 生成.beam文件
output_file(TermList, ConfName, MapType) ->
    Forms = get_form(TermList, ConfName, MapType),
    {ok, _, Bytes, _Warnings} = compile:forms(Forms, [return]),
    ErlPath = conf_util:get_beam_path(ConfName),
    file:write_file(ErlPath, Bytes),
    ok.

%% @doc 生成.erl文件
output_source(TermList, ConfName, MapType) ->
    Forms = get_form(TermList, ConfName, MapType),
    ErlPath = conf_util:get_src_path(ConfName),
    file:write_file(ErlPath, erl_prettypr:format(erl_syntax:form_list(Forms))),
    ok.

%% @doc
get_form(TermList, ConfName, MapType) ->
    Dic = get_dict(TermList, MapType),
    List = dict:to_list(Dic),
    %% load template beam
    FantasyBeamFile = code:where_is_file("conf_temp.beam"),
    {ok, {_, [{abstract_code, {_, Forms}}]}} = beam_lib:chunks(FantasyBeamFile, [abstract_code]),
    filter_form(Forms, List, ConfName, []).

%% @doc
filter_form([], _TermList, _ConfName, Acc) ->
    lists:reverse(Acc);
filter_form([{attribute, L, file, {_, _}} | Tail], TermList, ConfName, Acc) ->
    filter_form(Tail, TermList, ConfName, [{attribute, L, file, {atom_to_list(ConfName) ++ ".erl", L}} | Acc]);
filter_form([{attribute, L, module, _} | Tail], TermList, ConfName, Acc) ->
    filter_form(Tail, TermList, ConfName, [{attribute, L, module, ConfName} | Acc]);
filter_form([{attribute, _, export, _} = Export | Tail], TermList, ConfName, Acc) ->
    filter_form(Tail, TermList, ConfName, [Export | Acc]);
filter_form([{function, L, find, 1, [CatchAll]} | Tail], TermList, ConfName, Acc) ->
    ClauseList = expand_find_function(TermList, CatchAll, []),
    filter_form(Tail, TermList, ConfName, [{function, L, find, 1, ClauseList} | Acc]);
filter_form([{function, L, list, 0, [DefaultClause]} | Tail], TermList, ConfName, Acc) ->
    ClauseList = expand_list_function(TermList, DefaultClause),
    filter_form(Tail, TermList, ConfName, [{function, L, list, 0, ClauseList} | Acc]);
filter_form([T | Tail], TermList, ConfName, Acc) ->
    filter_form(Tail, TermList, ConfName, [T|Acc]).

%% @doc
expand_find_function([], CatchAll, Acc) ->
    lists:reverse([CatchAll | Acc]);
expand_find_function([{Key, Val} | Tail], CatchAll, Acc) ->
    {clause, L, _, _, _} = CatchAll,
    Clause = {clause, L, [erl_parse:abstract(Key)], [], [erl_parse:abstract(Val)]},
    expand_find_function(Tail, CatchAll, [Clause | Acc]).

%% @doc
expand_list_function(TermList, DefaultClause) ->
    {clause, L, _, _, _} = DefaultClause,
    [{clause, L, [], [], [erl_parse:abstract(TermList)]}].

%% @doc 解析
get_dict(TermList, MapType) ->
    lists:foldl(fun(Term, Acc) ->
        case {MapType, Term} of
            {kv_con, {Key, Val}} -> %% 键值
                dict:store(Key, Val, Acc);
            {kv_list, {Key, Val}} ->
                dict:append(Key, Val, Acc);
            {rec_con, Record} ->    %% record
                Key = erlang:element(2, Record),
                dict:store(Key, Record, Acc);
            {rec_list, Record} ->
                Key = erlang:element(2, Record),
                dict:append(Key, Record, Acc);
            _ ->
                Acc
        end
    end, dict:new(), TermList).
