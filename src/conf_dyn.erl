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
-module(conf_dyn).
-author("Ryuu").

-include_lib("kernel/include/file.hrl").
-include("conf_dyn.hrl").

%% API
-export([
    find/2,
    list/1,
    reload_all/0,
    reload_config/1,
    reload_changed/0,
    reload_parallel/0,
    reload_changed_parallel/0
]).

%% ====================================================================
%% Helper Functions
%% ====================================================================

%% @doc 获取配置
find(Conf, Key) ->
    case Conf:get(Key) of
        undefined ->
            [];
        V ->
            [V]
    end.

%% @doc 列出配置文件的内容
list(Conf) ->
    case Conf:list() of
        undefined ->
            [];
        V ->
            V
    end.

%% ====================================================================
%% Reload Functions
%% ====================================================================

%% @doc 生成所有配置文件
reload_all() ->
    load_conf_dyn_app(),
    Maps = get_config_maps(),
    ?FOREACH(fun conf_gen:generate_file/1, Maps),
    ok.

%% @doc 并行生成所有配置文件
reload_parallel() ->
    load_conf_dyn_app(),
    Maps = get_config_maps(),
    conf_gen:parallel_generate(Maps).

%% @doc 生成一个
reload_config(ConfName) ->
    load_conf_dyn_app(),
    Maps = get_config_maps(),
    case lists:keyfind(ConfName, #conf_map.name, Maps) of
        #conf_map{} = Map ->
            conf_gen:generate_file(Map);
        _ ->
            {error, notexist}
    end.

%% @doc 仅生成有变化的配置文件
reload_changed() ->
    load_conf_dyn_app(),
    Maps = get_changed_maps(),
    ?FOREACH(fun conf_gen:generate_file/1, Maps),
    ok.

%% @doc 并行生成所有配置文件
reload_changed_parallel() ->
    load_conf_dyn_app(),
    Maps = get_changed_maps(),
    conf_gen:parallel_generate(Maps).


%% ====================================================================
%% Local Functions
%% ====================================================================

%% @doc
load_conf_dyn_app() ->
    _ = application:load(conf_dyn).

%% @doc 获取所有的配置文件列表
get_config_maps() ->
    lists:foldl(fun({ConfName, RelPath, MapType}, Acc) ->
        [#conf_map{name = ConfName, path = RelPath, type = MapType} | Acc];
        ({ConfName, RelPath}, Acc) ->
            [#conf_map{name = ConfName, path = RelPath} | Acc];
        (_, Acc) ->
            Acc
    end, [], conf_util:get_conf_map()).

%% @doc 获取修改过的配置文件列表
get_changed_maps() ->
    Maps = get_config_maps(),
    lists:filter(fun conf_need_update/1, Maps).

%% @doc 判断配置是否需要更新
conf_need_update(Map) ->
    #conf_map{name = ConfName, path = RelPath} = Map,
    BeamFile = conf_util:get_beam_path(ConfName),
    ConfPath = conf_util:get_config_path(RelPath),
    case file:read_file_info(BeamFile) of
        {ok, #file_info{mtime = TBeam}} ->
            case file:read_file_info(ConfPath) of
                {ok, #file_info{mtime = Time}} when Time < TBeam ->
                    false;
                _ ->
                    true
            end;
        _ ->
            true
    end.
