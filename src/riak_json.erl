
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
    is_enabled/0,
    store_document/3,
    get_document/2,
    delete_document/2,
    link_schema/2,
    store_schema/2,
    get_schema/1,
    get_default_schema/1,
    delete_default_schema/1,
    find/2,
    index_exists/1,
    get_objects/1
    ]).

-include("riak_json.hrl").

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

%%% =================================================== internal functions

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
