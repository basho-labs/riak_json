%% @doc Test rj auth in various ways.
-module(rj_auth_test).

-compile(export_all).
-ifdef(integration_test).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
rj_auth_test_() ->
    {setup,
        fun () -> ok end,
        fun (_) -> ok end,
        {timeout, 60, [
            expected_statuses(rjt:use_auth())
        ]}
    }.

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
expected_statuses(false) ->
    [];
expected_statuses(true) ->
    [
    ?_assertMatch({ok,"404",_,_}, rjt:http(get, rjt:url("/idontexist"))),
    ?_assertMatch({ok,"401",_,_}, rjt:http(get, rjt:url("/idontexist"), <<>>, [], false)),
    ?_assertMatch({ok,"403",_,_}, rjt:http(get, rjt:url("/idontexist"), <<>>, [{"x-auth-token", invalid_auth_token()}], false))
    ].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

invalid_auth_token() ->
    "2161d12a3615392abee15e410bc0f865".

-endif.