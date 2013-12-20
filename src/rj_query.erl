
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

-module(rj_query).
-export([
        from_json/2
    ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

from_json({struct, Doc}, QueryType) ->
    GroupCatKeys = get_keys_with_prefixes([<<"$group">>, 
                                            <<"$categorize">>], Doc, []),

    PerPage = case QueryType of 
        one -> 1;
        _ -> 100
    end,

    QueryMods = [{<<"$page">>, 0},
                 {<<"$per_page">>, PerPage},
                 {<<"$sort">>, undefined}] ++ GroupCatKeys,
    {QueryModifiers, PropsLessQueryMods} = fetch_keys_and_delete_from_proplist(QueryMods, Doc, []),
    
    QueryPart = [{"q", join_query_parts({struct, PropsLessQueryMods})}],
    ModifierPart = process_query_modifier_parts(QueryModifiers, []),
    DefaultPart = default_solr_params(),
    lists:flatten([QueryPart, ModifierPart, DefaultPart]).


%%% =================================================== internal functions

default_solr_params() ->
    [
        {"omitHeader", true},
        {"wt", "json"},
        {"fl", "_yz_rb,_yz_rk"}
    ].

join_query_parts(QueryParts) ->
    SolrNodes = visit_query_nodes(QueryParts, []),
    string:join(SolrNodes, " OR ").

process_query_modifier_parts([], Params) ->
    lists:reverse(lists:flatten(Params));
process_query_modifier_parts([{Key, Value}|OtherModifiers], Params) when Value =/= undefined->
    case Key of
        <<"$group">> ->
            Param = populate_group_list(Value);
        <<"$categorize">> ->
            Param = populate_categorize_list(Value);
        <<"$page">> ->
            Param = {"start", Value};
        <<"$per_page">> ->
            Param = {"rows", Value};
        <<"$sort">> ->
            Param = {"sort", to_solr_sort(Value)};
        _ ->
            TruncKey = binary:part(Key, 1, byte_size(Key) - 1),
            Param = {binary_to_list(TruncKey), binary_to_list(Value)}
    end,
    process_query_modifier_parts(OtherModifiers, [Param | Params]);
process_query_modifier_parts([_|OtherModifiers], Params) ->
    process_query_modifier_parts(OtherModifiers, Params).

to_solr_sort(undefined) ->
    undefined;
to_solr_sort({struct, [{Field, Value}]}) ->
    binary_to_list(Field) ++ " " ++ case Value of
        1 -> "asc";
        -1 -> "desc";
        <<"asc">> -> "asc";
        <<"desc">> -> "desc";
        _ -> throw(unsupported_sort_option)
    end;
to_solr_sort(Sort) when is_list(Sort) ->
    string:join(
        lists:map(
            fun(Tuple) ->
                to_solr_sort(Tuple)
            end,
            Sort),
        ", ").

populate_group_list(undefined) ->
    undefined;
populate_group_list(Fields) ->
    populate_group_list(Fields, [{"group", "true"}|[]]).

populate_group_list([], GroupProps) ->
    lists:reverse(lists:flatten(GroupProps));
populate_group_list([{struct, GroupSpec}|Rest], GroupProps) ->
    populate_group_list(Rest, [parse_group_spec(GroupSpec, []) | GroupProps]).

parse_group_spec([], Acc) ->
    Acc;
parse_group_spec([Spec|GroupSpec], Acc) ->
    GroupProp = case Spec of
        {<<"field">>, Value} -> 
            {"group.field", binary_to_list(Value)};
        {<<"queries">>, Value} ->
            parse_query_list(Value, "group", []);
        {<<"rows">>, Value} ->
            {"rows", Value};
        {<<"limit">>, Value} ->
            {"group.limit", Value};
        {<<"sort">>, Value} ->
            {"group.sort", to_solr_sort(Value)};
        {<<"start">>, Value} ->
            {"group.start", Value};
        {SpecField, SpecValue} ->
            lager:info("Groupspec unknown field ~p:~p~n", [SpecField, SpecValue])
    end,
    parse_group_spec(GroupSpec, [GroupProp | Acc]).

parse_query_list([], _Prefix, Acc) ->
    Acc;
parse_query_list([Query|OtherQueries], Prefix, Acc) ->
    SolrQuery = string:join(visit_query_nodes(Query, []), "&"),
    parse_query_list(OtherQueries, Prefix, [{Prefix ++ ".query", SolrQuery}|Acc]).

populate_categorize_list(undefined) ->
    undefined;
populate_categorize_list(Categories) ->
    populate_categorize_list(Categories, [{"facet", "true"}|[]]).

populate_categorize_list([], CatProps) ->
    lists:reverse(lists:flatten(CatProps));
populate_categorize_list([{struct, CatSpec}|Rest], CatProps) ->
    populate_categorize_list(Rest, [parse_categorize_spec(CatSpec, []) | CatProps]).

parse_categorize_spec([], Acc) ->
    Acc;
parse_categorize_spec([Spec|CatSpec], Acc) ->
    CatProp = case Spec of
        {<<"field">>, Value} -> 
            {"facet.field", binary_to_list(Value)};
        {<<"queries">>, Value} ->
            parse_query_list(Value, "facet", []);
        {<<"range">>, {struct, Value}} ->
            parse_range_spec(Value, []);
        {<<"limit">>, Value} ->
            {"facet.limit", Value};
        {<<"sort">>, Value} ->
            {"facet.sort", binary_to_list(Value)};
        {<<"start">>, Value} ->
            {"facet.start", Value};
        {<<"stats">>, {struct, Value}} ->
            parse_categorize_stats_spec(Value, []);
        {SpecField, SpecValue} ->
            lager:info("Facetspec unknown field ~p:~p~n", [SpecField, SpecValue])
    end,
    parse_categorize_spec(CatSpec, [CatProp | Acc]).

parse_range_spec([], Acc) ->
    Acc;
parse_range_spec([Spec|RangeSpecs], Acc) ->
    RangeProp = case Spec of
        {<<"field">>, Value} ->
            {"facet.range", binary_to_list(Value)};
        {<<"start">>, Value} ->
            {"facet.range.start", Value};
        {<<"end">>, Value} ->
            {"facet.range.end", Value};
        {<<"increment">>, Value} ->
            {"facet.range.gap", Value}
    end,
    parse_range_spec(RangeSpecs, [RangeProp | Acc]).

parse_categorize_stats_spec([], StatsSpec) ->
    lists:reverse([{"stats","true"} | StatsSpec]);
parse_categorize_stats_spec([Spec|CatStatSpecs], StatsSpec) ->
    CatStatProp = case Spec of
        {<<"field">>, Value} ->
            {"stats.facet", binary_to_list(Value)};
        {<<"calculate">>, Value} ->
            {"stats.field", binary_to_list(Value)}
    end,
    parse_categorize_stats_spec(CatStatSpecs, [CatStatProp | StatsSpec]).


visit_query_nodes({struct, []}, SolrNodes) ->
    SolrNodes;
visit_query_nodes({struct, [{Op, NestedParams}|T]}, SolrNodes) when Op =:= <<"$and">> orelse Op =:= <<"$or">> ->
    [nested_from_json(Op, NestedParams) |
     visit_query_nodes({struct, T}, SolrNodes)];
visit_query_nodes({struct, [{Prop, ValueTuple}|T]}, SolrNodes) when is_tuple(ValueTuple) ->
    {struct, [{Op, Value}]} = ValueTuple,
    [operator(Op, Prop, Value) |
     visit_query_nodes({struct, T}, SolrNodes)];
visit_query_nodes({struct, [{Prop, Value}|T]}, SolrNodes) ->
    [token_to_string(Prop) ++ ":" ++ value_to_string(Value) |
     visit_query_nodes({struct, T}, SolrNodes)].

nested_from_json(Op, Doc) ->
%    lager:info("nested_from_json:~n  Op: ~p~n  Doc: ~p~n", [Op, Doc]),
    Conjunction =
        case Op of
            <<"$and">> -> "AND";
            <<"$or">>  -> "OR"
        end,
    lists:flatten(wrap_in_parentheses(
        string:join(
            lists:map(
                fun(N) -> visit_query_nodes(N, []) end,
                Doc),
            [wrap_in_spaces(Conjunction)]))).

operator(<<"$ne">>, P, V) ->
    "-" ++ token_to_string(P) ++ ":" ++ value_to_string(V);
operator(<<"$in">>, P, V) ->
    expand_to_conjunction(P, V, undefined, "OR");
operator(<<"$nin">>, P, V) ->
    expand_to_conjunction(P, V, "-", "AND");
operator(<<"$gt">>, P, V) when is_integer(V)->
    prefix_prop(P, "[" ++ token_to_string(V + 1) ++ " TO *]");
operator(<<"$lt">>, P, V) when is_integer(V)->
    prefix_prop(P, "[* TO " ++ token_to_string(V - 1) ++ "]");
operator(<<"$gte">>, P, V) ->
    prefix_prop(P, "[" ++ token_to_string(V) ++ " TO *]");
operator(<<"$lte">>, P, V) ->
    prefix_prop(P, "[* TO " ++ token_to_string(V) ++ "]");
operator(<<"$between">>, P, [From, To]) ->
    prefix_prop(P, "[" ++ token_to_string(From) ++ " TO " ++ token_to_string(To) ++ "]");
operator(<<"$regex">>, P, V) ->
    token_to_string(P) ++ ":" ++ token_to_string(V);
operator(<<"$exists">>, P, V) ->
    prefix_for_exists_op(V) ++ token_to_string(P) ++ ":[* TO *]".

wrap_in_parentheses(V) ->
    "(" ++ V ++ ")".

wrap_in_spaces(V) ->
    " " ++ V ++ " ".

prefix_for_exists_op(V) when V ->
    "";
prefix_for_exists_op(_) ->
    "-".

prefix_prop(P, Str) ->
    token_to_string(P) ++ ":" ++ Str.

expand_to_conjunction(P, V, TermPrefix, ConjunctionOp) ->
    Prefix = fetch_prefix_or_empty_string(TermPrefix),
    string:join(
        lists:map(
            fun(M) -> Prefix  ++ token_to_string(P) ++ ":" ++ value_to_string(M) end,
            V),
        wrap_in_spaces(ConjunctionOp)).

fetch_prefix_or_empty_string(Prefix) when is_list(Prefix) ->
    Prefix;
fetch_prefix_or_empty_string(_) ->
    "".

token_to_string(V) when is_binary(V) ->
    binary_to_list(V);
token_to_string(V) when not is_binary(V) ->
    lists:flatten(io_lib:format("~p", [V])).

value_to_string(V) when is_binary(V) ->
    "\"" ++ token_to_string(V) ++ "\"";
value_to_string(V) ->
    token_to_string(V).

fetch_and_delete_from_proplist(Key, PropList, Default) ->
    {
        proplists:get_value(Key, PropList, Default),
        proplists:delete(Key, PropList)
    }.

get_keys_with_prefixes([], _Params, Acc) ->
    lists:flatten(lists:reverse(Acc));
get_keys_with_prefixes([Prefix|OtherPrefixes], Params, Acc) ->
    Keys = get_keys_with_prefix(Prefix, Params, []),
    get_keys_with_prefixes(OtherPrefixes, Params, [Keys | Acc]).

get_keys_with_prefix(_, [], Keys) ->
    lists:reverse(Keys);
get_keys_with_prefix(Prefix, [{Key,DefaultValue}|Rest], Keys) when byte_size(Key) >= byte_size(Prefix) ->
    KeyPiece = binary:part(Key, 0, byte_size(Prefix)),
    if KeyPiece == Prefix ->
            get_keys_with_prefix(Prefix, Rest, [{Key, DefaultValue} | Keys]);
        true -> 
            get_keys_with_prefix(Prefix, Rest, Keys)
    end;
get_keys_with_prefix(Prefix, [_|Rest], Keys) ->
    get_keys_with_prefix(Prefix, Rest, Keys).

fetch_keys_and_delete_from_proplist([], PropList, Result) ->
    {lists:reverse(Result), PropList};
fetch_keys_and_delete_from_proplist([{Key, Default}|OtherKeys], PropList, Result) ->
    {Value, PropListLessKey} = fetch_and_delete_from_proplist(Key, PropList, Default),
    fetch_keys_and_delete_from_proplist(OtherKeys, PropListLessKey, [{Key, Value} | Result]).
    

-ifdef(TEST).

parse_group_arg_test() ->
    Json = "{\"$group\": [{\"field\": \"name\",
                          \"queries\": [{\"$and\": [{\"age\": {\"$gte\": 1}}, {\"age\": {\"$lte\": 10}}]},
                                        {\"age\": {\"$gte\": 11}}],
                          \"rows\": 10,
                          \"limit\": 3,
                          \"sort\": [{\"name\": 1}, {\"age\": -1}],
                          \"start\": 0}]}",
    {struct, ParsedJson} = mochijson2:decode(Json),
    GroupList = proplists:get_value(<<"$group">>, ParsedJson),
    ProcessedGroup = populate_group_list(GroupList),
    Expected = [{"group", "true"},
                {"group.field", "name"},
                {"group.query", "(age:[1 TO *] AND age:[* TO 10])"},
                {"group.query", "age:[11 TO *]"},
                {"rows", 10},
                {"group.limit", 3},
                {"group.sort", "name asc, age desc"},
                {"group.start", 0} 
                ],

    ?assertEqual(Expected, ProcessedGroup).

parse_categorize_arg_test() ->
    Json = "{\"$categorize\": [{\"field\": \"name\",
                               \"queries\": [{\"$and\": [{\"age\": {\"$gte\": 1}}, {\"age\": {\"$lte\": 10}}]},
                                             {\"age\": {\"$gte\": 11}}],
                               \"range\": {\"field\": \"birthday\",
                                           \"start\": 20, 
                                           \"end\": 30, 
                                           \"increment\": 1},
                               \"limit\": 3,
                               \"sort\": \"count\",
                               \"start\": 0,
                               \"stats\": {\"field\": \"state\",
                                           \"calculate\": \"age\"}
                               }]}",
    {struct, ParsedJson} = mochijson2:decode(Json),
    CatList = proplists:get_value(<<"$categorize">>, ParsedJson),
    ProcessedCategories = populate_categorize_list(CatList),
    Expected = [{"facet", "true"},
                {"facet.field", "name"},
                {"facet.query", "(age:[1 TO *] AND age:[* TO 10])"},
                {"facet.query", "age:[11 TO *]"},
                {"facet.range", "birthday"},
                {"facet.range.start", 20},
                {"facet.range.end", 30},
                {"facet.range.gap", 1},
                {"facet.limit", 3},
                {"facet.sort", "count"},
                {"facet.start", 0}, 
                {"stats", "true"},
                {"stats.field", "age"},
                {"stats.facet", "state"}
                ],

    io:format("~p~n~p", [Expected, ProcessedCategories]),
    ?assertEqual(Expected, ProcessedCategories).


get_keys_with_prefix_test() ->
    Prefix = <<"$facet">>,
    PropList = [{<<"$facet">>, true}, 
                {<<"$facet.field">>, <<"count">>}, 
                {<<"$facet.count.min">>, 1}, 
                {<<"$not_facet">>, <<"other">>}],
    PrefixedKeys = get_keys_with_prefix(Prefix, PropList, []),
    ExpectedKeys = [{<<"$facet">>, true}, 
                    {<<"$facet.field">>, <<"count">>},
                    {<<"$facet.count.min">>, 1}],

    ?assertEqual(ExpectedKeys, PrefixedKeys).

get_keys_with_prefixes_test() ->
    Prefixes = [<<"$facet">>, <<"$stat">>],
    PropList = [{<<"$facet.field">>, <<"count">>}, 
                {<<"$stat.field">>, <<"age">>}, 
                {<<"$not_facet">>, <<"other">>}],
    PrefixedKeys = get_keys_with_prefixes(Prefixes, PropList, []),
    ExpectedKeys = [{<<"$facet.field">>, <<"count">>},
                    {<<"$stat.field">>, <<"age">>}],

    ?assertEqual(ExpectedKeys, PrefixedKeys).

fetch_keys_and_delete_from_proplist_test() ->
    PropList = [{<<"$facet">>, true}, 
                {<<"$facet.field">>, <<"count">>}, 
                {<<"$facet.count.min">>, 1}, 
                {<<"$not_facet">>, <<"other">>}],
    Keys = [{<<"$facet">>, undefined}, 
            {<<"$facet.field">>, undefined}, 
            {<<"$facet.count.min">>, undefined}],

    ExpectedFacetPropList = [{<<"$facet">>, true}, 
                             {<<"$facet.field">>, <<"count">>}, 
                             {<<"$facet.count.min">>, 1}],
    ExpectedOtherPropList = [{<<"$not_facet">>, <<"other">>}],

    {FacetPropList, OtherPropList} = fetch_keys_and_delete_from_proplist(Keys, PropList, []),

    ?assertEqual(FacetPropList, ExpectedFacetPropList),
    ?assertEqual(OtherPropList, ExpectedOtherPropList).

process_query_modifier_parts_test() ->
    Doc = [{<<"$per_page">>, 99},
           {<<"$page">>, 7}, 
           {<<"$sort">>, [{struct, [{<<"count">>, 1}]}, {struct, [{<<"age">>, -1}]}]}
           % {<<"$stats">>, [<<"count">>]},
           % {<<"$facets">>, [<<"name">>]}
           ],
    Expected = [{"rows", 99},
                {"start", 7}, 
                {"sort", "count asc, age desc"}],
                % {"stats", "true"},
                % {"stats.field", "count"},
                % {"facet", "true"},
                % {"facet.field", "name"}],
    Actual = process_query_modifier_parts(Doc, []),

    ?assertEqual(Expected, Actual).

equality_test() ->
    Input = mochijson2:decode(
         "{\"foo\": 42}"),
    Expected = "foo:42",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).

value_is_string_test() ->
    Input = mochijson2:decode(
         "{\"foo\": \"this is a string\"}"),
    Expected = "foo:\"this is a string\"",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).

gte_test() ->
    Input = mochijson2:decode("{\"foo\": {\"$gte\": 42}}"),
    Expected = "foo:[42 TO *]",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).

lte_test() ->
    Input = mochijson2:decode("{\"foo\": {\"$lte\": 42}}"),
    Expected = "foo:[* TO 42]",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).

gt_int_test() ->
    Input = mochijson2:decode("{\"foo\": {\"$gt\": 42}}"),
    Expected = "foo:[43 TO *]",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).

lt_int_test() ->
    Input = mochijson2:decode("{\"foo\": {\"$lt\": 42}}"),
    Expected = "foo:[* TO 41]",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).
    
ne_test() ->
    Input = mochijson2:decode("{\"foo\": {\"$ne\": 42}}"),
    Expected = "-foo:42",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).

in_test() ->
    Input = mochijson2:decode("{\"foo\": {\"$in\": [4, 8, 15, 16, 23, 42]}}"),
    Expected = "foo:4 OR foo:8 OR foo:15 OR foo:16 OR foo:23 OR foo:42",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).

%% Not the Nine Inch Nails test. That would be way more awesome.
nin_test() ->
    Input = mochijson2:decode("{\"foo\": {\"$nin\": [4, 8, 15, 16, 23, 42]}}"),
    Expected = "-foo:4 AND -foo:8 AND -foo:15 AND -foo:16 AND -foo:23 AND -foo:42",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).

and_test() ->
    Input = mochijson2:decode("{\"$and\": [{\"foo\": 42}, {\"bar\":43}]}"),
    Expected = "(foo:42 AND bar:43)",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).

or_test() ->
    Input = mochijson2:decode("{\"$or\": [{\"foo\": 42}, {\"bar\":43}]}"),
    Expected = "(foo:42 OR bar:43)",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).
    
three_expression_test() ->
    Input = mochijson2:decode(
         "{\"foo\": {\"$ne\": 42},
              \"bar\": {\"$lt\": 5},
              \"blech\": 42}"),
    Expected = "-foo:42 OR bar:[* TO 4] OR blech:42",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).

nested_and_test() ->
    Input = mochijson2:decode("{\"blech\": 44, \"$and\": [{\"foo\": 42}, {\"bar\":43}]}"),
    Expected = "blech:44 OR (foo:42 AND bar:43)",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).
    
exists_test() ->
    Input = mochijson2:decode("{\"foo\": {\"$exists\": true}}"),
    Expected = "foo:[* TO *]",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).

neg_exists_test() ->
    Input = mochijson2:decode("{\"foo\": {\"$exists\": false}}"),
    Expected = "-foo:[* TO *]",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).

between_test() ->
    Input = mochijson2:decode("{\"foo\": {\"$between\": [1, 10]}}"),
    Expected = "foo:[1 TO 10]",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).

regex_test() ->
    Input = mochijson2:decode("{\"foo\": {\"$regex\": \"/\\d{2}/\"}}"),
    Expected = "foo:/\\d{2}/",
    Actual = proplists:get_value("q", from_json(Input, all)),
    ?assertEqual(Expected, Actual).

to_solr_sort_undefined_test() ->
    ?assertEqual(undefined, to_solr_sort(undefined)).

to_solr_sort_tuple_test() ->
    Input = mochijson2:decode("{\"foo\": 1}"),
    ?assertEqual("foo asc", to_solr_sort(Input)).

to_solr_sort_string_tuple_test() ->
    Input = mochijson2:decode("[{\"foo\": \"asc\"},{\"bar\": \"desc\"}]"),
    ?assertEqual("foo asc, bar desc", to_solr_sort(Input)).

to_solr_sort_list_test() ->
    Input = mochijson2:decode("[{\"foo\": 1}, {\"bar\": -1}]"),
    ?assertEqual("foo asc, bar desc", to_solr_sort(Input)).

-endif.