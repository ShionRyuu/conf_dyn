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
-module(conf_gen_test).
-author("Ryuu").

-include("conf_dyn.hrl").

%% API
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

conf_gen_test() ->
    ?assertEqual(conf_dyn:reload_config(mora), ok),
    ?assertEqual(mora:get(invite_times), 5),
    ?assertEqual(mora:get(gain_times), 10),
    ?assertEqual(mora:get(not_exist_key), undefined),
    ?assertEqual(conf_dyn:find(mora, invite_times), [5]),
    ?assertEqual(conf_dyn:find(mora, gain_times), [10]),
    ?assertEqual(conf_dyn:find(mora, not_exist_key), []),
    ?assertEqual(?CONF_FIND(mora, invite_times), 5),
    ?assertEqual(?CONF_FIND(mora, gain_times), 10),
    ?assertEqual(?CONF_FIND(mora, not_exist_key), undefined),
    ok.

