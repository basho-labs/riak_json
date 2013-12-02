%% @doc Test the schema API in various ways.
-module(rj_wm_schema_test).

-compile(export_all).
-ifdef(integration_test).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TEST DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
rj_wm_schema_test_() ->
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
    ?_assertMatch({ok,"404",_}, rjt:http(get, rjt:url("doesntexist","/schema"))),
    {timeout, 60, [?_assertMatch({ok,"204",_}, rjt:http(put, rjt:url("soontoexist","/schema"), valid_schema()))]}
    ].

expected_data() ->
    {ok,"204",[]} = rjt:http(put, rjt:url("soontoexist","/schema"), valid_schema()),
    {ok,"200",Body} = rjt:http(get, rjt:url("soontoexist","/schema")),
    [
    ?_assertEqual(mochijson2:decode(valid_schema()), mochijson2:decode(Body)),
    {timeout, 60, [?_assertMatch({ok,"204",_}, rjt:http(delete, rjt:url("soontoexist","/schema")))]},
    ?_assertMatch({ok,"404",_}, rjt:http(get, rjt:url("soontoexist","/schema"))) %% delete doesn't actually delete index yet for some reason
    ].
    % [].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
valid_schema() ->
    <<"[{\"name\": \"user_name\", \"type\": \"string\"},
        {\"name\": \"full_name\", \"type\": \"text\"},
        {\"name\": \"categories\", \"type\": \"multi_string\"},
        {\"name\": \"age\", \"type\": \"number\"}]">>.

-endif.
