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
            expected_data()
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


% require_relative "./acceptance_test_helper">>),

% describe "An AND query, when there is a match" do
%   before { clear_buckets }

%   let(:document) { {"first_name" => "Evan", "last_name" => "Light"} }
%   let(:json)     { JSON.parse(response.body) }
%   let(:response) {
%     RestClient.post(
%       "#{COLLECTION_URL}/people",
%       document.to_json,
%       {content_type: :json}
%     )

%     sleep 1

%     RestClient.put(
%       "#{COLLECTION_URL}/people/query/all",
%       {"$and" => [{:first_name => "Evan"}, {:last_name => "Light"}]}.to_json,
%       content_type: :json
%     )
%   }

%   it "returns a 200" do
%     assert_equal(200, response.code)
%   end

%   it "returns the matched record with an _id field" do
%     returned_doc = json["data"].first
%     assert(returned_doc.delete("_id"), "No _id present")
%     assert_equal(document, returned_doc)
%   end

%   describe "the result header" do
%     it "has a total of 1" do
%       assert_equal(1, json["total"])
%     end
%   end
% end






% require_relative './acceptance_test_helper'

% describe "Paginated /query/all result" do
%   def perform_query(args = {"$per_page" => 1, "$page" => 1})
%     response = RestClient.put(
%       "#{COLLECTION_URL}/foobar/query/all",
%       {first_name: "Evan"}.merge(args).to_json,
%       content_type: :json
%     )
%     assert_equal(200, response.code)
%     JSON.parse(response.body)
%   end

%   before do
%     clear_buckets
%   end

%   describe "for empty results sets" do
%     before do
%       RestClient.post(
%         "#{COLLECTION_URL}/foobar",
%         {first_name: "Bogus"}.to_json,
%         content_type: :json
%       )
%     end

%     let(:json) { perform_query }

%     it "has a page of 0" do
%       assert_equal(0, json["page"])
%     end

%     it "has a num_pages of 0" do
%       assert_equal(0, json["num_pages"])
%     end
%   end

%   describe "for non-empty result sets" do
%     before do
%       document = {first_name: "Evan", last_name: "Light"}
%       2.times do
%         RestClient.post(
%           "#{COLLECTION_URL}/foobar",
%           document.to_json,
%           {content_type: :json}
%         )
%       end
%       sleep 1
%     end

%     let(:json) { perform_query("$per_page" => 1, "$page" => 1) }

%     describe "when total returned is just greater than the per_page" do
%       it "has a page of 1" do
%         assert_equal(1, json["page"])
%       end

%       it "has a num_pages of 2" do
%         assert_equal(2, json["num_pages"])
%       end

%       it "contains only the first page's results" do
%         assert_equal(1, json["data"].size)
%       end

%       describe "and requested the second page" do
%         before do
%           @page_1_obj_id = json["data"][0]["_id"]
%           @page_2_results = perform_query("$per_page" => 1, "$page" => 2)
%         end

%         it "has a page of 2" do
%           assert_equal(2, @page_2_results["page"])
%         end

%         it "has a num_pages of 2" do
%           assert_equal(2, @page_2_results["num_pages"])
%         end

%         it "contains only the second page's results" do
%           page_2_obj_id = @page_2_results["data"][0]["_id"]
%           assert(page_2_obj_id, "No record for second object")
%           refute_equal(@page_1_obj_id, page_2_obj_id, "Failed to get second page? Obj1 == Obj2")
%         end
%       end
%     end

%     describe "when total is equal to the requested per_page" do
%       let(:json) { perform_query("$per_page" => 2) }

%       it "has a page of 1" do
%         assert_equal(1, json["page"])
%       end

%       it "has a num_pages of 1" do
%         assert_equal(1, json["num_pages"])
%       end

%       it "has the expected total" do
%         assert_equal(2, json["total"])
%       end

%       it "returns a total that is equal to the requested per_page" do
%         assert_equal(json["total"], json["per_page"])
%       end
%     end

%     describe "when total is less than per_page" do
%       let(:json) { perform_query("$per_page" => 10) }

%       it "has a page of 1" do
%         assert_equal(1, json["page"])
%       end

%       it "has a num_pages of 1" do
%         assert_equal(1, json["num_pages"])
%       end
%     end
%   end
% end



% require_relative './acceptance_test_helper'

% describe "Sorted /query/all result" do
%   def perform_query(args = {})
%     begin
%       response = RestClient.put(
%         "#{COLLECTION_URL}/sort_test/query/all",
%         {"value" => {"$exists" => true}}.merge(args).to_json,
%         content_type: :json
%       )

%       assert_equal(200, response.code)
%       JSON.parse(response.body)
%     rescue
%       puts $!
%       fail
%     end
%   end

%   before do
%     clear_buckets

%     sleep 2

%     1.upto(5) do |i|
%       RestClient.post(
%         "#{COLLECTION_URL}/sort_test",
%         {"value" => i}.to_json,
%         {content_type: :json}
%       )
%     end

%     sleep 2
%   end

%   let(:sorted_ascending) { perform_query("$sort" => {"value" => 1}) }
%   let(:sorted_descending) { perform_query("$sort" => {"value"=> -1}) }

%   describe "when requested ascending sort" do
%     it "returns all of the values" do
%       assert_equal(5, sorted_ascending["data"].size)
%     end

%     it "returns values sorted ascending" do
%       values = sorted_ascending["data"].map { |o| o["value"] }
%       assert_equal([1,2,3,4,5], values)
%     end
%   end

%   describe "when requested descending sort" do
%     it "returns all of the values" do
%       assert_equal(5, sorted_descending["data"].size)
%     end

%     it "returns values sorted descending" do
%       values = sorted_descending["data"].map { |o| o["value"] }
%       assert_equal([5,4,3,2,1], values)
%     end
%   end
% end



% require_relative './acceptance_test_helper'

% describe "/query" do
%   before { clear_buckets }

%   describe "/query/one" do
%     it "200s with a JSON body of '[]' on no matches" do
%       document = {first_name: "Evan", last_name: "Light"}
%       json = document.to_json

%       RestClient.post(
%         "#{COLLECTION_URL}/foobar",
%         json,
%         {content_type: :json}
%       )

%       sleep 1

%       response = RestClient.put(
%         "#{COLLECTION_URL}/foobar/query/one",
%         {foo: "bar"}.to_json,
%         content_type: :json
%       )
%       assert_equal(200, response.code)
%       assert_equal([], JSON.parse(response.body),
%     end

%     describe "equality query" do
%       it "200s with the match when there is a match" do
%         collection = "#{COLLECTION_URL}/people/">>),
%         first_name = "Evan">>),
%         document = {"first_name" => "#{first_name}", "last_name" => "Light"}
%         json = document.to_json

%         RestClient.put(
%           "#{collection}/evan",
%           json,
%           {content_type: :json}
%         )

%         sleep 1

%         response = RestClient.put(
%           "#{collection}/query/one",
%           %Q/{ "first_name": "#{first_name}" }/,
%           content_type: :json
%         )

%         assert_equal(200, response.code)
%         assert_equal(document.merge("_id" => "evan"), JSON.parse(response.body),
%       end
%     end

%     describe "range query" do
%       it "200s with the match when there is a match" do
%         skip "Range queries will work once use of the default schema is implemented">>),
%         collection = "#{COLLECTION_URL}/people/">>),
%         first_name = "Evan">>),
%         document = {"first_name" => "Evan", "last_name" => "Light", "age" => 40}

%         RestClient.put(
%           "#{collection}/evan",
%           document.to_json,
%           {content_type: :json}
%         )

%         response = RestClient.put(
%           "#{collection}/query/one",
%           %Q/{ "age": { "$gt": 20 } }/,
%           content_type: :json
%         )

%         assert_equal(200, response.code)
%         assert_equal(document.merge("_id" => "evan"), JSON.parse(response.body),
%       end
%     end
%   end

%   describe "/query/all" do
%     describe "when there are no matches" do
%       let(:json)     { JSON.parse(response.body) }
%       let(:response) {
%         document = {first_name: "Evan", last_name: "Light"}.to_json

%         RestClient.post(
%           "#{COLLECTION_URL}/foobar",
%           document,
%           {content_type: :json}
%         )

%         query = <<-HERE
%          {"foo": "bar", "blech": "bla"}
%         HERE

%         RestClient.put(
%           "#{COLLECTION_URL}/foobar/query/all",
%           query,
%           content_type: :json
%         )
%       }

%       it "returns a 200" do
%         assert_equal(200, response.code)
%       end

%       it "returns an empty array as data" do
%         assert_equal([], json["data"])
%       end

%       describe "the result header" do
%         it "has a total of 0" do
%           assert_equal(0, json["total"])
%         end
%       end
%     end

%     describe "when there is a match" do
%       let(:document) { {"first_name" => "Evan", "last_name" => "Light"} }
%       let(:json)     { JSON.parse(response.body) }
%       let(:response) {
%         RestClient.post(
%           "#{COLLECTION_URL}/people",
%           document.to_json,
%           {content_type: :json}
%         )

%         sleep 1

%         RestClient.put(
%           "#{COLLECTION_URL}/people/query/all",
%           {first_name: "Evan"}.to_json,
%           content_type: :json
%         )
%       }

%       it "returns a 200" do
%         assert_equal(200, response.code)
%       end

%       it "returns the matched record with an _id field" do
%         returned_doc = json["data"].first
%         assert(returned_doc.delete("_id"), "No _id present")
%         assert_equal(document, returned_doc)
%       end

%       describe "the result header" do
%         it "has a total of 1" do
%           assert_equal(1, json["total"])
%         end
%       end
%     end
%   end
% end
