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
-module(conf_util).
-author("Ryuu").

%% API
-export([
    get_config_dir/0,
    get_config_path/1,
    get_beam_path/1,
    get_src_path/1,
    get_conf_map/0
]).

%% @doc 配置文件目录
get_config_dir() ->
    case application:get_env(conf_dyn, conf_dir) of
        {ok, Path} -> Path;
        _ -> "."
    end.

%% @doc 配置文件路径
get_config_path(RelPath) ->
    ConfDir = get_config_dir(),
    filename:join([ConfDir, RelPath]).

%% @doc .beam文件目录
get_beam_dir() ->
    case application:get_env(conf_dyn, beam_dir) of
        {ok, Path} -> Path;
        _ -> "."
    end.

%% @doc .beam文件路径
get_beam_path(ConfName) ->
    BeamDir = get_beam_dir(),
    filename:join([BeamDir, erlang:atom_to_list(ConfName) ++ ".beam"]).

%% @doc .beam文件目录
get_src_dir() ->
    case application:get_env(conf_dyn, src_dir) of
        {ok, Path} -> Path;
        _ -> "."
    end.

%% @doc .erl文件路径
get_src_path(ConfName) ->
    SrcDir = get_src_dir(),
    filename:join([SrcDir, erlang:atom_to_list(ConfName) ++ ".erl"]).

%% @doc 配置映射表
get_conf_map() ->
    case application:get_env(conf_dyn, conf_map) of
        {ok, L} when is_list(L) -> L;
        _ -> []
    end.
