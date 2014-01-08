%% @doc Test the query API in various ways.
-module(rj_wm_query_test).

-compile(export_all).
-ifdef(integration_test).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TEST DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
rj_wm_schema_test_() ->
    {setup,
        fun () -> ok end,
        fun (_) -> rjt:clear("searchcol") end,
        {timeout, 60, [
            expected_data(),
            tombstones()
        ]}
    }.

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

expected_data() ->
    {ok, "204", _} = rjt:http(put, rjt:url("searchcol","/Petunia"), <<"{\"name\": \"Petunia\", \"metric\": 31}">>),
    {ok, "204", _} = rjt:http(put, rjt:url("searchcol","/Max"), <<"{\"name\": \"Max\", \"metric\": 2}">>),
    {ok, "204", _} = rjt:http(put, rjt:url("searchcol","/Carrie"), <<"{\"name\": \"Carrie\", \"metric\": 28}">>),
    {ok, "204", _} = rjt:http(put, rjt:url("searchcol","/Wilt"), <<"{\"name\": \"Wilt\", \"metric\": 28}">>),
    {ok, "204", _} = rjt:http(put, rjt:url("searchcol","/Roberta"), <<"{\"name\": \"Roberta\", \"metric\": 2}">>),
    {ok, "204", _} = rjt:http(put, rjt:url("searchcol","/Rowena"), <<"{\"name\": \"Rowena\", \"metric\": 2}">>),
    {ok, "204", _} = rjt:http(put, rjt:url("searchcol","/Robert"), <<"{\"name\": \"Robert\", \"metric\": 40}">>),
    {ok, "204", _} = rjt:http(put, rjt:url("searchcol","/Casey"), <<"{\"name\": \"Casey\", \"metric\": 9000}">>),
    {ok, "204", _} = rjt:http(put, rjt:url("searchcol","/Drew"), <<"{\"name\": \"Drew\", \"metric\": 1}">>),
    {ok, "204", _} = rjt:http(put, rjt:url("searchcol","/Dan"), <<"{\"name\": \"Dan\", \"metric\": 2}">>),
    {ok, "204", _} = rjt:http(put, rjt:url("searchcol","/Felix"), <<"{\"name\": \"Felix\", \"metric\": 3}">>),

    %% Make sure index is created and docs have time to get indexed for the first time
    timer:sleep(3000),

    [
     ?_assertEqual({ok, "200", query_one_result()},                    rjt:http(put, rjt:url("searchcol","/query/one"), query_one())),
     ?_assertEqual({ok, "200", query_one_regex_result()},              rjt:http(put, rjt:url("searchcol","/query/one"), query_one_regex())),
     ?_assertEqual({ok, "200", query_all_gt_result()},                 rjt:http(put, rjt:url("searchcol","/query/all"), query_all_gt())),
     ?_assertEqual({ok, "200", query_all_lte_result()},                rjt:http(put, rjt:url("searchcol","/query/all"), query_all_lte())),
     ?_assertEqual({ok, "200", query_all_categorize_field_result()},   rjt:http(put, rjt:url("searchcol","/query/all"), query_all_categorize_field())),
     %% ?_assertEqual({ok, "200", query_all_categorize_query_result()},   rjt:http(put, rjt:url("searchcol","/query/all"), query_all_categorize_query())),
     ?_assertEqual({ok, "200", query_all_categorize_range_result()},   rjt:http(put, rjt:url("searchcol","/query/all"), query_all_categorize_range())),
     ?_assertEqual({ok, "200", query_all_group_field_result()},        rjt:http(put, rjt:url("searchcol","/query/all"), query_all_group_field())),
     %% ?_assertEqual({ok, "200", query_all_group_query_result()},        rjt:http(put, rjt:url("searchcol","/query/all"), query_all_group_query())),
     ?_assertEqual({ok, "200", query_all_or_result()},                 rjt:http(put, rjt:url("searchcol","/query/all"), query_all_or())),
     ?_assertEqual({ok, "200", query_all_page_1_result()},             rjt:http(put, rjt:url("searchcol","/query/all"), query_all_page_1())),
     ?_assertEqual({ok, "200", query_all_page_2_result()},             rjt:http(put, rjt:url("searchcol","/query/all"), query_all_page_2())),
     ?_assertEqual({ok, "200", query_all_regex_result()},              rjt:http(put, rjt:url("searchcol","/query/all"), query_all_regex())),
     ?_assertEqual({ok, "200", query_all_sort_asc_result()},           rjt:http(put, rjt:url("searchcol","/query/all"), query_all_sort_asc())),
     ?_assertEqual({ok, "200", query_all_sort_desc_result()},          rjt:http(put, rjt:url("searchcol","/query/all"), query_all_sort_desc()))
    ].

tombstones() ->
    {ok, "204", _} = rjt:http(put, rjt:url("searchcol2","/Drew"), <<"{\"name\": \"Drew\", \"metric\": 1}">>),
    {ok, "204", _} = rjt:http(delete, rjt:url("searchcol2","/Drew")),

    [
     ?_assertEqual({ok, "200", query_one_result()},                    rjt:http(put, rjt:url("searchcol","/query/one"), query_one())),
     ?_assertEqual({ok, "200", query_one_regex_result()},              rjt:http(put, rjt:url("searchcol","/query/one"), query_one_regex())),
     ?_assertEqual({ok, "200", query_all_gt_result()},                 rjt:http(put, rjt:url("searchcol","/query/all"), query_all_gt())),
     ?_assertEqual({ok, "200", query_all_lte_result()},                rjt:http(put, rjt:url("searchcol","/query/all"), query_all_lte())),
     ?_assertEqual({ok, "200", query_all_categorize_field_result()},   rjt:http(put, rjt:url("searchcol","/query/all"), query_all_categorize_field())),
     %% ?_assertEqual({ok, "200", query_all_categorize_query_result()},   rjt:http(put, rjt:url("searchcol","/query/all"), query_all_categorize_query())),
     ?_assertEqual({ok, "200", query_all_categorize_range_result()},   rjt:http(put, rjt:url("searchcol","/query/all"), query_all_categorize_range())),
     ?_assertEqual({ok, "200", query_all_group_field_result()},        rjt:http(put, rjt:url("searchcol","/query/all"), query_all_group_field())),
     %% ?_assertEqual({ok, "200", query_all_group_query_result()},        rjt:http(put, rjt:url("searchcol","/query/all"), query_all_group_query())),
     ?_assertEqual({ok, "200", query_all_or_result()},                 rjt:http(put, rjt:url("searchcol","/query/all"), query_all_or())),
     ?_assertEqual({ok, "200", query_all_page_1_result()},             rjt:http(put, rjt:url("searchcol","/query/all"), query_all_page_1())),
     ?_assertEqual({ok, "200", query_all_page_2_result()},             rjt:http(put, rjt:url("searchcol","/query/all"), query_all_page_2())),
     ?_assertEqual({ok, "200", query_all_regex_result()},              rjt:http(put, rjt:url("searchcol","/query/all"), query_all_regex())),
     ?_assertEqual({ok, "200", query_all_sort_asc_result()},           rjt:http(put, rjt:url("searchcol","/query/all"), query_all_sort_asc())),
     ?_assertEqual({ok, "200", query_all_sort_desc_result()},          rjt:http(put, rjt:url("searchcol","/query/all"), query_all_sort_desc()))
    ].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
query_one() ->
    "{\"name\": \"Drew\"}".

query_one_result() ->
    "{\"_id\":\"Drew\",\"name\":\"Drew\",\"metric\":1}".

query_one_regex() ->
    "{\"name\": {\"$regex\": \"\/C.*\/\"}}".

query_one_regex_result() ->
    "{\"_id\":\"Carrie\",\"name\":\"Carrie\",\"metric\":28}".

query_all_gt() ->
    "{\"metric\": {\"$gt\": 2}}".

query_all_gt_result() ->
    "{\"total\":6,\"page\":0,\"per_page\":100,\"num_pages\":1,\"data\":[{\"_id\":\"Petunia\",\"name\":\"Petunia\",\"metric\":31},{\"_id\":\"Carrie\",\"name\":\"Carrie\",\"metric\":28},{\"_id\":\"Wilt\",\"name\":\"Wilt\",\"metric\":28},{\"_id\":\"Robert\",\"name\":\"Robert\",\"metric\":40},{\"_id\":\"Casey\",\"name\":\"Casey\",\"metric\":9000},{\"_id\":\"Felix\",\"name\":\"Felix\",\"metric\":3}]}".

query_all_lte() ->
    "{\"metric\": {\"$lte\": 5}}".

query_all_lte_result() ->
    "{\"total\":6,\"page\":0,\"per_page\":100,\"num_pages\":1,\"data\":[{\"_id\":\"Max\",\"name\":\"Max\",\"metric\":2},{\"_id\":\"Roberta\",\"name\":\"Roberta\",\"metric\":2},{\"_id\":\"Rowena\",\"name\":\"Rowena\",\"metric\":2},{\"_id\":\"Drew\",\"name\":\"Drew\",\"metric\":1},{\"_id\":\"Dan\",\"name\":\"Dan\",\"metric\":2},{\"_id\":\"Felix\",\"name\":\"Felix\",\"metric\":3}]}".

query_all_categorize_field() ->
    "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$per_page\": 0, \"\$categorize\": [{\"field\": \"metric\"}]}".

query_all_categorize_field_result() ->
    "{\"categories\":{\"metric\":{\"2.0\":4,\"28.0\":2,\"1.0\":1,\"3.0\":1,\"31.0\":1,\"40.0\":1,\"9000.0\":1}},\"total\":11,\"page\":0,\"per_page\":0,\"num_pages\":1,\"data\":[]}".

query_all_categorize_query() ->
    "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$per_page\": 0, \"\$categorize\": [{\"queries\": [{\"name\": {\"\$regex\": \"/R.*/\"}}, {\"name\": {\"\$regex\": \"/.*a/\"}}]}]}".

query_all_categorize_query_result() ->
    "<html><head><title>500 Internal Server Error</title></head><body><h1>Internal Server Error</h1>The server encountered an error while processing this request:<br><pre>{error,\n    {error,function_clause,\n        [{rj_query_response,flatten_facet_results,\n             [[{struct,[{<<\"name:/.*a/\">>,3},{<<\"name:/R.*/\">>,3}]}],[]],\n             [{file,\"src/rj_query_response.erl\"},{line,133}]},\n         {rj_query_response,parse_results,3,\n             [{file,\"src/rj_query_response.erl\"},{line,47}]},\n         {rj_query_response,process_results,3,\n             [{file,\"src/rj_query_response.erl\"},{line,28}]},\n         {rj_query_response,format_json_response,3,\n             [{file,\"src/rj_query_response.erl\"},{line,14}]},\n         {rj_wm_query,accept_json,2,[{file,\"src/rj_wm_query.erl\"},{line,52}]},\n         {webmachine_resource,resource_call,3,\n             [{file,\"src/webmachine_resource.erl\"},{line,186}]},\n         {webmachine_resource,do,3,\n             [{file,\"src/webmachine_resource.erl\"},{line,142}]},\n         {webmachine_decision_core,resource_call,1,\n             [{file,\"src/webmachine_decision_core.erl\"},{line,48}]}]}}</pre><P><HR><ADDRESS>mochiweb+webmachine web server</ADDRESS></body></html>".

query_all_categorize_range() ->
    "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$per_page\": 0, \"\$categorize\": [{\"range\": {\"field\": \"metric\", \"start\": 1, \"end\": 50, \"increment\": 10}}]}".

query_all_categorize_range_result() ->
    "{\"categories\":{\"metric\":{\"1.0\":6,\"11.0\":0,\"21.0\":2,\"31.0\":2,\"41.0\":0}},\"total\":11,\"page\":0,\"per_page\":0,\"num_pages\":1,\"data\":[]}".

query_all_group_field() ->
    "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$group\": [{\"field\": \"metric\", \"limit\": 10, \"start\": 1}]}".

query_all_group_field_result() ->
    "{\"groups\":{\"metric\":{\"3.0\":[],\"1.0\":[],\"9000.0\":[],\"40.0\":[],\"28.0\":[],\"2.0\":[],\"31.0\":[]}}}".

query_all_group_query() ->
    "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$group\": [{\"queries\": [{\"name\": {\"\$regex\": \"/R.*/\"}}, {\"name\": {\"\$regex\": \"/.*a/\"}}], \"start\": 1}]}".

query_all_group_query_result() ->
%%    {ok,"204",[]}.
    [].

query_all_or() ->
    "{\"$or\": [{\"name\": {\"$regex\": \"/D.*/\"}},{\"name\": {\"$regex\": \"/F.*/\"}}]}".

query_all_or_result() ->
    "{\"total\":3,\"page\":0,\"per_page\":100,\"num_pages\":1,\"data\":[{\"_id\":\"Drew\",\"name\":\"Drew\",\"metric\":1},{\"_id\":\"Dan\",\"name\":\"Dan\",\"metric\":2},{\"_id\":\"Felix\",\"name\":\"Felix\",\"metric\":3}]}".

query_all_page_1() ->
    "{\"name\": {\"$regex\": \"/.*/\"}, \"$per_page\": 1}".

query_all_page_1_result() ->
    "{\"total\":11,\"page\":0,\"per_page\":1,\"num_pages\":11,\"data\":[{\"_id\":\"Petunia\",\"name\":\"Petunia\",\"metric\":31}]}".

query_all_page_2() ->
    "{\"name\": {\"$regex\": \"/.*/\"}, \"$per_page\": 1, \"$page\": 2}".

query_all_page_2_result() ->
    "{\"total\":11,\"page\":2,\"per_page\":1,\"num_pages\":11,\"data\":[{\"_id\":\"Carrie\",\"name\":\"Carrie\",\"metric\":28}]}".

query_all_regex() ->
    "{\"name\": {\"$regex\": \"/D.*/\"}}".

query_all_regex_result() ->
    "{\"total\":2,\"page\":0,\"per_page\":100,\"num_pages\":1,\"data\":[{\"_id\":\"Drew\",\"name\":\"Drew\",\"metric\":1},{\"_id\":\"Dan\",\"name\":\"Dan\",\"metric\":2}]}".

query_all_sort_asc() ->
    "{\"name\": {\"$regex\": \"/.*/\"}, \"$sort\": {\"metric\": 1}}".

query_all_sort_asc_result() ->
    "{\"total\":11,\"page\":0,\"per_page\":100,\"num_pages\":1,\"data\":[{\"_id\":\"Drew\",\"name\":\"Drew\",\"metric\":1},{\"_id\":\"Max\",\"name\":\"Max\",\"metric\":2},{\"_id\":\"Roberta\",\"name\":\"Roberta\",\"metric\":2},{\"_id\":\"Rowena\",\"name\":\"Rowena\",\"metric\":2},{\"_id\":\"Dan\",\"name\":\"Dan\",\"metric\":2},{\"_id\":\"Felix\",\"name\":\"Felix\",\"metric\":3},{\"_id\":\"Carrie\",\"name\":\"Carrie\",\"metric\":28},{\"_id\":\"Wilt\",\"name\":\"Wilt\",\"metric\":28},{\"_id\":\"Petunia\",\"name\":\"Petunia\",\"metric\":31},{\"_id\":\"Robert\",\"name\":\"Robert\",\"metric\":40},{\"_id\":\"Casey\",\"name\":\"Casey\",\"metric\":9000}]}".

query_all_sort_desc() ->
    "{\"name\": {\"$regex\": \"/.*/\"}, \"$sort\": {\"metric\": -1}}".

query_all_sort_desc_result() ->
    "{\"total\":11,\"page\":0,\"per_page\":100,\"num_pages\":1,\"data\":[{\"_id\":\"Casey\",\"name\":\"Casey\",\"metric\":9000},{\"_id\":\"Robert\",\"name\":\"Robert\",\"metric\":40},{\"_id\":\"Petunia\",\"name\":\"Petunia\",\"metric\":31},{\"_id\":\"Carrie\",\"name\":\"Carrie\",\"metric\":28},{\"_id\":\"Wilt\",\"name\":\"Wilt\",\"metric\":28},{\"_id\":\"Felix\",\"name\":\"Felix\",\"metric\":3},{\"_id\":\"Max\",\"name\":\"Max\",\"metric\":2},{\"_id\":\"Roberta\",\"name\":\"Roberta\",\"metric\":2},{\"_id\":\"Rowena\",\"name\":\"Rowena\",\"metric\":2},{\"_id\":\"Dan\",\"name\":\"Dan\",\"metric\":2},{\"_id\":\"Drew\",\"name\":\"Drew\",\"metric\":1}]}".

-endif.