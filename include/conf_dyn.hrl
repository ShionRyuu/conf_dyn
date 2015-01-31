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
-author("Ryuu").

-define(FOREACH(Fun, List), lists:foreach(fun(E) -> Fun(E) end, List)).

%% 获取配置
-define(CONF_FIND(Conf, Key), Conf:get(Key)).
-define(CONF_LIST(Conf, Key), Conf:list(Key)).
-define(CONF_DEFAULT(Conf, Key, Default), case Conf:get(Key) of undefined -> Default; V -> V end).

%% @doc 格式{配置名, 配置路径, 格式[kv_con, kv_list, rec_con, rec_list]}
-record(conf_map, {name, path, type = kv_con}).

%% @doc 配置{配置存放路径}
-record(config, {conf_dir, beam_dir}).
