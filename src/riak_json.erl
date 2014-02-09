
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

%% @doc riak_json external api

-module(riak_json).
-export([
    store_document/3,
    get_document/2,
    delete_document/2,
    link_schema/2,
    store_schema/2,
    get_schema_name/1,
    get_schema/1,
    get_default_schema/1,
    delete_default_schema/1,
    find/2,
    index_exists/1,
    get_objects/1,
    get_collections/0,
    get_collections/1
    ]).

-include("riak_json.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

store_document(Collection, Key, JDocument) ->
    maybe_create_schema(Collection, JDocument, rj_yz:index_exists(Collection)),
    rj_yz:put(Collection, Key, JDocument).

get_document(Collection, Key) ->
    rj_yz:get(Collection, Key).

delete_document(Collection, Key) ->
    rj_yz:delete(Collection, Key).

link_schema(Collection, SchemaName) ->
    rj_yz:create_index(Collection, SchemaName).

delete_default_schema(Collection) ->
    case index_exists(Collection) of
        true -> rj_yz:delete_index(Collection);
        _ -> {error, notfound}
    end.

store_schema(SchemaName, JSchema) ->
    XMLSchema = rj_schema:to_xml(JSchema),
    rj_yz:store_schema(SchemaName, XMLSchema).

get_default_schema(Collection) ->
    case index_exists(Collection) of
        true -> get_schema(?RJ_SCHEMA(Collection));
        _ -> {error, notfound}
    end.

get_schema_name(Collection) ->
    ?RJ_SCHEMA(Collection).

get_schema(SchemaName) ->
    XMLSchema = rj_yz:get_schema(SchemaName),

    case XMLSchema of
        {error, Reason} -> {error, Reason};
        XML -> rj_schema:from_xml(XML)
    end.

find(Collection, JQuery) ->
    rj_yz:perform_query(Collection, JQuery).

index_exists(Collection) ->
    rj_yz:index_exists(Collection).

get_objects(BucketKeyList) ->
    get_objects(BucketKeyList, []).

get_collections() ->
    get_collections(<<>>).

% Filter on collections like "prefix.collection"
get_collections(AnyPrefix) ->
    Prefix = rj_util:any_to_binary(AnyPrefix),
    get_collections(Prefix, rj_yz:bucket_type_list(), []).

get_collections(_, [], Cols) ->
    lists:reverse(Cols);
get_collections(Prefix, [{_, false, _}| R], Cols) ->
    get_collections(Prefix, R, Cols);
get_collections(<<>>=Prefix, [{Name, true, _}| R], Cols) ->
    get_collections(Prefix, R, [{name, Name} | Cols]);
get_collections(Prefix, [{Name, true, _}| R], Cols) ->
    case is_rj_collection(Prefix, Name) of
        true -> get_collections(Prefix, R, [{name, Name} | Cols]);
        false -> get_collections(Prefix, R, Cols)
    end.

%%% =================================================== internal functions

is_rj_collection(P, N) when is_binary(P)->
    is_rj_collection(rj_util:any_to_list(P), rj_util:any_to_list(N));
is_rj_collection(P, N) -> 
    PrefixMatch = case P of
        [] -> true;
        _ -> has_string(N,P)
    end,
    (has_string(N, ?RJ_TYPE_POSTFIX) and PrefixMatch).

has_string(Haystack, Needle) ->
    (string:str(Haystack, Needle) > 0).

maybe_create_schema(Collection, JDocument, false) ->
    DefaultSchemaName = ?RJ_SCHEMA(Collection),
    JsonSchema = rj_schema:from_document(JDocument),
    store_schema(DefaultSchemaName, JsonSchema),
    link_schema(Collection, ?RJ_SCHEMA(Collection)),
    ok;
maybe_create_schema(_, _, _) ->
    ok.

get_objects([], Objects) ->
    lists:reverse(Objects);
get_objects([{Bucket, Key}|Others], Acc) ->
    case rj_yz:get(Bucket, Key) of
        undefined -> get_objects(Others, Acc);
        Object -> get_objects(Others, [{Key, Object} | Acc])
    end.

-ifdef(TEST).

collections_test() ->
    Name = ?RJ_TYPE("testing"),
    PreName = "mydb." ++ Name,
    ?assertEqual(true, is_rj_collection(<<>>, Name)),
    ?assertEqual(true, is_rj_collection(<<"mydb">>, PreName)),
    ?assertEqual(false, is_rj_collection(<<>>, <<"default">>)).
-endif.