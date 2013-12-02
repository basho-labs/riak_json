
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

-module(rj_yz).
-export([
    get/2,
    put/3,
    delete/2,
    get_schema/1,
    store_schema/2,
    index_exists/1,
    create_index/2,
    perform_query/2,
    delete_index/1
]).

get(Collection, Key) ->
    Result = yz_kv:get(
        yz_kv:client(),
        bucket_with_type_from(Collection),
        list_to_binary(Key)),

    case Result of
        {value, V} ->
            binary_to_list(V);
        _ -> undefined
    end.


put(Collection, Key, Doc) ->
    yz_kv:put(
        yz_kv:client(),
        bucket_with_type_from(Collection),
        list_to_binary(Key),
        Doc,
        "application/json").

delete(Collection, Key) ->
    C = yz_kv:client(),
    C:delete(bucket_with_type_from(Collection), list_to_binary(Key)).

get_schema(SchemaName) ->
    S1 = case SchemaName of
        "default" -> <<"_yz_default">>;
        S -> list_to_binary(S)
    end,

    case yz_schema:get(S1) of
        {ok, RawSchema} ->
            binary_to_list(RawSchema);
        {error, _, Reason} ->
            {error, Reason}
    end.

store_schema(SchemaName, XMLSchema) ->
    yz_schema:store(list_to_binary(SchemaName), list_to_binary(XMLSchema)).

index_exists(Collection) ->
    BucketType = bucket_type_from(Collection),

    Props = case riak_core_bucket_type:get(BucketType) of
        undefined -> [];
        P -> P
    end,

    case proplists:get_value(yz_index, Props, <<"_yz_default">>) of
        <<"_yz_default">> ->
            false;
        _ ->
            yz_index:exists(list_to_binary(Collection ++ "RJIndex"))
    end.


%% Reset bucket type
%% Delete the index
delete_index(Collection) ->
    IndexName = list_to_binary(Collection ++ "RJIndex"),
    BucketType = bucket_type_from(Collection),



    case riak_core_bucket_type:get(BucketType) of
        undefined -> ok;
        _ ->
            riak_core_bucket_type:update(BucketType, [{yz_index, <<"_yz_default">>}])
    end,

    yz_index:remove(IndexName).

%% Create an index for a created schema
%% Create / Update the RJ bucket type for this collection
%% May need to have a different predictable index / schema name
create_index(Collection, SchemaName) ->
    IndexName = list_to_binary(Collection ++ "RJIndex"),
    BucketType = bucket_type_from(Collection),

    yz_index:create(IndexName, list_to_binary(SchemaName)),
    wait_for({yz_solr, ping, [IndexName]}, 5),

    case riak_core_bucket_type:get(BucketType) of
        undefined ->
            riak_core_bucket_type:create(BucketType, [{allow_mult, false},{yz_index, IndexName}]),
            riak_core_bucket_type:activate(BucketType);
        _ ->
            riak_core_bucket_type:update(BucketType, [{allow_mult, false},{yz_index, IndexName}])
    end.

perform_query(Collection, Query) ->
    lager:info("YZ query: ~p~n", [Query]),
    lager:info("Formatted query: ~p~n", [mochiweb_util:urlencode(Query)]),
    yz_solr:dist_search(list_to_binary(Collection ++ "RJIndex"), Query).

%%% =================================================== internal functions

%% @doc Wait for `Check' for the given number of `Seconds'.
wait_for(_, 0) ->
    ok;
wait_for(Check={M,F,A}, Seconds) when Seconds > 0 ->
    case M:F(A) of
        true ->
            ok;
        false ->
            timer:sleep(1000),
            wait_for(Check, Seconds - 1)
    end.

bucket_type_from(Collection) ->
    list_to_binary(Collection ++ "RJType").

bucket_with_type_from(Collection) ->
    {bucket_type_from(Collection), list_to_binary(Collection)}.
