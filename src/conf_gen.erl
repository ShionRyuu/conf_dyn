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
            io:format("receive unknown msg:~p~n", [_Other]),
            do_wait_worker(N, Ref)
    end.

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
        {ok, TermList, FullName} ->
            output_file(TermList, ConfName, FullName, MapType);
        {error, Error} ->
            {error, Error}
    end.

%% @doc 生成Erl文件
output_file(TermList, ConfName, FullName, MapType) ->
    Content = get_source(TermList, ConfName, MapType),
    case catch dynamic_compile:from_string(Content) of
        {_, Bin} ->
            %% 写.beam文件
            Path = conf_util:get_beam_path(ConfName),
            file:write_file(Path, Bin, [write, binary]),
            %% 加载
            code:load_binary(ConfName, FullName, Bin),
            ok;
        {_, _, Error} ->
            {error, Error}
    end.

%% @doc 构造源代码
get_source(TermList, ConfName, MapType) ->
    Dic = get_dict(TermList, MapType),
%%     Default = if MapType =:= set -> undefined; ture -> [] end,
    format("-module(~w).~n-export([get/1, list/0]).~n~sget(_) -> undefined.~nlist() -> ~w.~n", [
        ConfName, [io_lib:format("get(~w) -> ~w;~n", [Key, Val]) || {Key, Val} <- dict:to_list(Dic)], TermList]).

%% @doc 格式化
format(Temp, Content) ->
    lists:flatten(io_lib:format(Temp, Content)).

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
