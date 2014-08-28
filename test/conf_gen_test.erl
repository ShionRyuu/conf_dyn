%%%-------------------------------------------------------------------
%%% @author Ryuu
%%% @copyright (C) 2014, <PLANT>
%%% @doc
%%%     测试
%%% @end
%%%-------------------------------------------------------------------
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

