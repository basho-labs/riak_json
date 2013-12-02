%% @doc Test the document API in various ways.
-module(rj_wm_document_test).

-compile(export_all).
-ifdef(integration_test).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TEST DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
rj_wm_document_test_() ->
    {setup,
        fun () -> rjt:clear() end,
        fun (_) -> ok end,
        {timeout, 60, [
            expected_statuses(),
            expected_data()
        ]}
    }.

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

expected_statuses() ->
    [
    ?_assertMatch({ok,"404",_}, rjt:http(get, rjt:url("/idontexist"))),
    {timeout, 60, [?_assertMatch({ok,"204",_}, rjt:http(put, rjt:url("/iwillexist"), valid_doc()))]},
    ?_assertMatch({ok,"405",_}, rjt:http(put, rjt:url("/"), valid_doc())),
    ?_assertMatch({ok,"204",_}, rjt:http(post, rjt:url("/itoowillexist"), valid_doc())),
    ?_assertMatch({ok,"204",_}, rjt:http(delete, rjt:url("/itoowillexist"))),
    ?_assertMatch({ok,"404",_}, rjt:http(delete, rjt:url("/idontexist"))),
    ?_assertMatch({ok,"200",_}, rjt:http(get, rjt:url("/iwillexist")))
    ].

expected_data() ->
    {ok, Status, Headers,_} = rjt:http(post, rjt:url("/"), valid_doc(), [], rjt:use_auth(), true),
    ReRes = re:run(proplists:get_value("Location", Headers),
        "document/collection/" ++ rjt:test_bucket()),

    [
    ?_assertEqual("201", Status),
    ?_assertEqual({match,[{0,28}]}, ReRes)
    ].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
valid_doc() ->
    <<"{\"name\": \"Drew Kerrigan\"}">>.

-endif.
