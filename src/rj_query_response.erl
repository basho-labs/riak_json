
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(rj_query_response).

-export([format_json_response/3,
		 key_object_pair_to_mochijson/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


format_json_response(JsonResults, QueryType, SolrQuery) ->
	{struct, Results} = mochijson2:decode(JsonResults),
	ProcessedResults = process_results(Results, QueryType, SolrQuery),
	mochijson2:encode(ProcessedResults).

%% internal functions

process_results(Results, one, _SolrQuery)->
    {struct, Response} = proplists:get_value(<<"response">>, Results),
    Objects = get_objects(Response),
    case length(Objects) of
        0 -> Objects;
        _ -> key_object_pair_to_mochijson(lists:nth(1, Objects))
    end;
process_results(Results, _, SolrQuery) ->
    {struct, lists:flatten(parse_results(Results, SolrQuery, []))}.


parse_results([], _SolrQuery, ProcResults) ->
    % Chance for 2 category props to make it in, reduce
    Categories = proplists:get_all_values("categories", ProcResults),
    case Categories of
        [] -> ProcResults;
        _ -> [{"categories", merge_proplists(Categories, [])}|proplists:delete("categories", ProcResults)]
    end;
parse_results([Result|Rest], SolrQuery, ProcResults) ->
    ResultProp = case Result of
        {<<"response">>, {struct, Response}} ->
            process_object_response(Response, SolrQuery);
        {<<"grouped">>, {struct, Grouped}} ->
            {"groups", process_group_results(Grouped, [])};
        {<<"stats">>, {struct, Stats}} ->
            {"categories", process_facet_results(Stats, [])};
        {<<"facet_counts">>, {struct, FacetCounts}} ->
            {"categories", process_facet_results(FacetCounts, [])}
    end,
    parse_results(Rest, SolrQuery, [ResultProp | ProcResults]).

merge_proplists([], Categories) ->
    {struct, lists:merge(Categories)};
merge_proplists([{struct, Data}|Rest], Categories) ->
    merge_proplists(Rest, [Data|Categories]).

process_object_response(Response, SolrQuery) ->
    Objects = get_objects(Response),
    RowsPerPage = proplists:get_value("rows", SolrQuery),
    Total = proplists:get_value(<<"numFound">>, Response),
    PageCount = case RowsPerPage of
        0 -> 1;
        _ ->
            mochinum:int_ceil(Total/RowsPerPage)
    end,
    Page = case Total > 0 of
        false -> 0;
        true -> proplists:get_value("start", SolrQuery)
    end,

    ResultAttributes = [
        {total, Total},
        {page, Page},
        {per_page, RowsPerPage},
        {num_pages, PageCount},
        {
            data, 
            lists:map(fun ?MODULE:key_object_pair_to_mochijson/1, Objects)
        }],
    ResultAttributes.

process_group_results([], GroupResults) ->
    GroupResults;
process_group_results([{Query, {struct, QuerySpec}}|Rest], GroupResults) ->
    ResultProp = case proplists:is_defined(<<"groups">>, QuerySpec) of
        true -> 
            Groups = proplists:get_value(<<"groups">>, QuerySpec),
            {Query, extract_grouped_docs(Groups, [])};
        false ->
            {Query, extract_docs(QuerySpec)}
    end,
    process_group_results(Rest, [ResultProp | GroupResults]).

extract_grouped_docs([], Results) ->
    {struct, Results};
extract_grouped_docs([{struct, Group}|Rest], Results) ->
    GroupValue = proplists:get_value(<<"groupValue">>, Group),
    GroupLabel = 
        if 
            is_integer(GroupValue) ->
                lists:nth(1, io_lib:format("~p", [GroupValue])) ;
            is_float(GroupValue) -> 
                lists:nth(1, io_lib:format("~.1f", [GroupValue]));
            true -> binary_to_list(GroupValue)
        end,
    Docs = extract_docs(Group),
    extract_grouped_docs(Rest, [{GroupLabel, Docs}|Results]).

extract_docs(DocListSpec) ->
    {struct, DocList} = proplists:get_value(<<"doclist">>, DocListSpec, undefined),
    Objects = get_objects(DocList),
    lists:map(fun ?MODULE:key_object_pair_to_mochijson/1, Objects).

process_facet_results([], FacetResults) ->
    flatten_facet_results(lists:flatten(FacetResults), []);
process_facet_results([{FacetType, {struct, FacetData}}|Rest], FacetResults) ->
    CatProp = case FacetType of
        <<"facet_queries">> ->
            {struct, [{<<"Queries">>,FacetData}]};
        <<"facet_fields">> ->
            process_field_data(FacetData);
        <<"facet_dates">> ->
            process_range_data(FacetData, []);
        <<"facet_ranges">> ->
            process_range_data(FacetData, []);
        <<"stats_fields">> ->
            process_stats_data(FacetData)
    end,
    if 
        CatProp =:= {struct, []}; CatProp =:= {struct, [{<<"Queries">>, []}]} ->
            process_facet_results(Rest, FacetResults);
        true ->
            process_facet_results(Rest, [CatProp|FacetResults])
    end.

flatten_facet_results([], Results) ->
    {struct, lists:reverse(Results)};
flatten_facet_results([{struct, [Result]}|Rest], Results) ->
    flatten_facet_results(Rest, [Result|Results]).

process_field_data([]) ->
    [];
process_field_data([{FieldName, Data}]) ->
    {struct, [{FieldName, process_field_data(Data, [])}]}.

process_field_data([], Fields) ->
    {struct, lists:reverse(Fields)};
process_field_data([Key, Value|Rest], Fields) ->
    process_field_data(Rest, [{Key, Value}|Fields]).

process_range_data([], Ranges) ->
    lists:flatten(Ranges);
process_range_data([{RangeId, {struct, RangeData}}|Rest], Ranges) ->
    Counts = proplists:get_value(<<"counts">>, RangeData),
    process_range_data(Rest, [{struct, [{RangeId, process_field_data(Counts, [])}]}|Ranges]).

process_stats_data([{_StatsId, null}]) ->
    [];
process_stats_data([{_StatsId, {struct, StatsData}}]) ->
    proplists:get_value(<<"facets">>, StatsData).

bucket_keys_from_yz(Parsed) ->
    YzBucketKeys = proplists:get_value(<<"docs">>, Parsed),
    lists:map(fun({struct, DocProps}) ->
                {binary_to_list(proplists:get_value(<<"_yz_rb">>, DocProps)),
                 binary_to_list(proplists:get_value(<<"_yz_rk">>, DocProps))}
            end, 
            YzBucketKeys).

get_objects(Results) ->
    BucketKeys = bucket_keys_from_yz(Results),
    riak_json:get_objects(BucketKeys).

key_object_pair_to_mochijson({Key, Object}) ->
    {struct, DecodedObject} = mochijson2:decode(Object),
    {struct, [{<<"_id">>, list_to_binary(Key)} | DecodedObject]}.


-ifdef(TEST).

bucket_keys_from_yz_test() ->
    Json = "{\"docs\": [{\"_yz_rb\": \"Bucket1\", \"_yz_rk\": \"Key1\"}]}",
    {struct, Docs} = mochijson2:decode(Json),

    Expected = [{"Bucket1", "Key1"}],
    Result = bucket_keys_from_yz(Docs),
    ?assertEqual(Expected, Result).


process_facet_results_field_test() ->
    Data = [{<<"facet_queries">>,{struct,[]}},{<<"facet_fields">>,{struct,[{<<"metric">>,[<<"2">>,4,<<"28">>,2,<<"1">>,1,<<"3">>,1,<<"31">>,1,<<"33">>,1,<<"40">>,1,<<"9000">>,1]}]}}],
    Expected = {struct,[{<<"metric">>,{struct, [{<<"2">>,4},{<<"28">>,2},{<<"1">>,1},{<<"3">>,1},{<<"31">>,1},{<<"33">>,1},{<<"40">>,1},{<<"9000">>,1}]}}]},
    Result = process_facet_results(Data, []),
    ?assertEqual(Expected, Result).

process_facet_results_query_test() ->
    Data = [{<<"facet_queries">>,{struct,[{<<"name:/.*a/">>,3},{<<"name:/R.*/">>,3}]}},{<<"facet_fields">>,{struct,[]}},{<<"facet_dates">>,{struct,[]}},{<<"facet_ranges">>,{struct,[]}}],
    Expected = {struct,[{<<"Queries">>, [{<<"name:/.*a/">>,3}, {<<"name:/R.*/">>,3}]}]},
    Result = process_facet_results(Data, []),
    ?assertEqual(Expected, Result).

process_facet_results_range_test() ->
    Data = [{<<"facet_queries">>,{struct,[]}},{<<"facet_fields">>,{struct,[]}},{<<"facet_dates">>,{struct,[]}},{<<"facet_ranges">>,{struct,[{<<"metric">>,{struct,[{<<"counts">>,[<<"1">>,6,<<"11">>,0,<<"21">>,2,<<"31">>,3,<<"41">>,0]},{<<"gap">>,10},{<<"start">>,1},{<<"end">>,51}]}}]}}],

    Expected = {struct,[{<<"metric">>,{struct, [{<<"1">>,6},{<<"11">>,0},{<<"21">>,2},{<<"31">>,3},{<<"41">>,0}]}}]},
    Result = process_facet_results(Data, []),
    ?assertEqual(Expected, Result).


% format_Results_test() ->
%     Context = #ctx{
%         query_type=all,
%         solr_query=[{<<"rows">>, 10},
%                     {<<"start">>, 0}]
%     },
%     Json = "{\"numFound\":7,\"start\":0,\"maxScore\":1.2513144,
%             \"docs\":[{\"_id\":[\"0\"],\"_yz_id\":\"Acgbku9ZoGFJJHO4E8DVpf6QhA5_55\",\"_yz_ed\":\"55 test_collection Acgbku9ZoGFJJHO4E8DVpf6QhA5 g2IHF45D\",\"_yz_fpn\":\"54\",\"_yz_node\":\"riak@127.0.0.1\",\"_yz_pn\":\"55\",\"_yz_rk\":\"Acgbku9ZoGFJJHO4E8DVpf6QhA5\",\"_yz_rb\":\"test_collection\"},
%             {\"_yz_id\":\"Z5fmbPN4NT8Wx0MOzzAVEtWFu84_31\",\"_yz_ed\":\"31 test_collection Z5fmbPN4NT8Wx0MOzzAVEtWFu84 g2IDoTs4\",\"_yz_fpn\":\"29\",\"_yz_node\":\"riak@127.0.0.1\",\"_yz_pn\":\"31\",\"_yz_rk\":\"Z5fmbPN4NT8Wx0MOzzAVEtWFu84\",\"_yz_rb\":\"test_collection\"},
%             {\"_yz_id\":\"SKzkzG7scq5cMHiuu0CusjgWLJ8_40\",\"_yz_ed\":\"40 test_collection SKzkzG7scq5cMHiuu0CusjgWLJ8 g2IAlXKV\",\"_yz_fpn\":\"39\",\"_yz_node\":\"riak@127.0.0.1\",\"_yz_pn\":\"40\",\"_yz_rk\":\"SKzkzG7scq5cMHiuu0CusjgWLJ8\",\"_yz_rb\":\"test_collection\"},
%             {\"_yz_id\":\"Nm9TcwniwyH3zxISh5NVtNNCIKl_7\",\"_yz_ed\":\"7 test_collection Nm9TcwniwyH3zxISh5NVtNNCIKl g2IElCR+\",\"_yz_fpn\":\"7\",\"_yz_node\":\"riak@127.0.0.1\",\"_yz_pn\":\"7\",\"_yz_rk\":\"Nm9TcwniwyH3zxISh5NVtNNCIKl\",\"_yz_rb\":\"test_collection\"},
%             {\"_yz_id\":\"P4GxtVZWaMD2ju0rH8AHj6tstNM_61\",\"_yz_ed\":\"61 test_collection P4GxtVZWaMD2ju0rH8AHj6tstNM g2ICrQGX\",\"_yz_fpn\":\"59\",\"_yz_node\":\"riak@127.0.0.1\",\"_yz_pn\":\"61\",\"_yz_rk\":\"P4GxtVZWaMD2ju0rH8AHj6tstNM\",\"_yz_rb\":\"test_collection\"},
%             {\"_yz_id\":\"key_55\",\"_yz_ed\":\"55 test_collection key g2IG7HqO\",\"_yz_fpn\":\"55\",\"_yz_node\":\"riak@127.0.0.1\",\"_yz_pn\":\"55\",\"_yz_rk\":\"key\",\"_yz_rb\":\"test_collection\"},
%             {\"_yz_id\":\"W1GtDP448LUF5IoDiQEINOcnkc7_16\",\"_yz_ed\":\"16 test_collection W1GtDP448LUF5IoDiQEINOcnkc7 g2ID6GVK\",\"_yz_fpn\":\"16\",\"_yz_node\":\"riak@127.0.0.1\",\"_yz_pn\":\"16\",\"_yz_rk\":\"W1GtDP448LUF5IoDiQEINOcnkc7\",\"_yz_rb\":\"test_collection\"}]}",
%     {struct, Decoded} = mochijson2:decode(Json),

%     Results = process_results(Decoded, Context),
%     io:format("Processed Restuls: ~p~n", [Results]),
%     ?assertEqual(proplists:get_value(<<"total">>, Results), 7),
%     ?assertEqual(proplists:get_falue(<<"rows">>, Results), 10).


-endif.

