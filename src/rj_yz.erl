
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
    delete_index/1,
    bucket_type_list/0
]).

-include("riak_json.hrl").
-include_lib("yokozuna/include/yokozuna.hrl").

get(Collection, Key) ->
    lager:debug("Collection: ~p, Key: ~p~n", [Collection, Key]),

    Result = yz_kv:get(
        yz_kv:client(),
        bucket_with_type_from(Collection),
        rj_util:any_to_binary(Key)),

    lager:debug("Result: ~p~n", [Result]),

    case Result of
        {value, V} ->
            binary_to_list(V);
        _ -> undefined
    end.

put(Collection, Key, Doc) ->
    lager:debug("Collection: ~p, Key: ~p, Doc: ~p~n", [Collection, Key, Doc]),

    Result = yz_kv:put(
        yz_kv:client(),
        bucket_with_type_from(Collection),
        rj_util:any_to_binary(Key),
        Doc,
        "application/json"),

    lager:debug("Result: ~p~n", [Result]),

    Result.

delete(Collection, Key) ->
    lager:debug("Collection: ~p, Key: ~p~n", [Collection, Key]),

    C = yz_kv:client(),
    Result = C:delete(bucket_with_type_from(Collection), rj_util:any_to_binary(Key)),

    lager:debug("Result: ~p~n", [Result]),

    Result.

get_schema(SchemaName) ->
    lager:debug("SchemaName: ~p~n", [SchemaName]),

    S1 = case SchemaName of
        "default" -> ?YZ_DEFAULT_SCHEMA_NAME;
        S -> rj_util:any_to_binary(S)
    end,

    Result = case yz_schema:get(S1) of
        {ok, RawSchema} ->
            binary_to_list(RawSchema);
        {error, _, Reason} ->
            {error, Reason}
    end,

    lager:debug("Result: ~p~n", [Result]),

    Result.

store_schema(SchemaName, XMLSchema) ->
    lager:debug("SchemaName: ~p, XMLSchema: ~p~n", [SchemaName, XMLSchema]),

    Result = yz_schema:store(rj_util:any_to_binary(SchemaName), rj_util:any_to_binary(XMLSchema)),

    lager:debug("Result: ~p~n", [Result]),

    Result.

index_exists(Collection) ->
    lager:debug("Collection: ~p~n", [Collection]),

    BucketType = bucket_type_from(Collection),

    Props = case riak_core_bucket_type:get(BucketType) of
        undefined -> [];
        P -> P
    end,

    Result = case proplists:get_value(?YZ_INDEX, Props, ?YZ_DEFAULT_SCHEMA_NAME) of
        ?YZ_DEFAULT_SCHEMA_NAME ->
            false;
        _ ->
            yz_index:exists(rj_util:any_to_binary(?RJ_INDEX(Collection)))
    end,

    lager:debug("Result: ~p~n", [Result]),

    Result.


%% Reset bucket type
%% Delete the index
delete_index(Collection) ->
    lager:debug("Collection: ~p~n", [Collection]),

    IndexName = rj_util:any_to_binary(?RJ_INDEX(Collection)),
    BucketType = bucket_type_from(Collection),

    case riak_core_bucket_type:get(BucketType) of
        undefined -> ok;
        _ ->
            riak_core_bucket_type:update(BucketType, [{?YZ_INDEX, ?YZ_DEFAULT_SCHEMA_NAME}])
    end,

    Result = yz_index:remove(IndexName),

    lager:debug("Result: ~p~n", [Result]),

    Result.

%% Create an index for a created schema
%% Create / Update the RJ bucket type for this collection
%% May need to have a different predictable index / schema name
create_index(Collection, SchemaName) ->
    lager:debug("Collection: ~p, SchemaName: ~p~n", [Collection, SchemaName]),

    IndexName = rj_util:any_to_binary(?RJ_INDEX(Collection)),
    BucketType = bucket_type_from(Collection),

    yz_index:create(IndexName, rj_util:any_to_binary(SchemaName)),

    wait_for({yz_solr, ping, [IndexName]}, 5),

    case riak_core_bucket_type:get(BucketType) of
        undefined ->
            riak_core_bucket_type:create(BucketType, [{allow_mult, false},{?YZ_INDEX, IndexName}]),
            riak_core_bucket_type:activate(BucketType);
        _ ->
            riak_core_bucket_type:update(BucketType, [{allow_mult, false},{?YZ_INDEX, IndexName}])
    end.

perform_query(Collection, Query) ->
    lager:debug("Collection: ~p, Query: ~p, Formatted query: ~p~n", [Collection, Query, mochiweb_util:urlencode(Query)]),
    Result = yz_solr:dist_search(list_to_binary(?RJ_INDEX(rj_util:any_to_list(Collection))), Query),

    lager:debug("Result: ~p~n", [Result]),

    Result.

bucket_type_list() ->
    It = riak_core_bucket_type:iterator(),
    bucket_type_list(It, []).

bucket_type_list(It, Types) ->
    case riak_core_bucket_type:itr_done(It) of
        true ->
            riak_core_bucket_type:itr_close(It),
            lists:reverse(Types);
        false ->
            {Type, Props} = riak_core_bucket_type:itr_value(It),
            IsActive = proplists:get_value(active, Props, false),
            bucket_type_list(riak_core_bucket_type:itr_next(It), [{Type, IsActive, Props} | Types])
    end.

%%% =================================================== internal functions

%% @doc Wait for `Check' for the given number of `Seconds'.
wait_for(_, 0) ->
    ok;
wait_for(Check={M,F,A}, Seconds) when Seconds > 0 ->
    case M:F(A) of
        true ->
            ok;
        false ->
            lager:debug("Waiting for ~p:~p(~p)...~n", [M, F, A]),
            timer:sleep(1000),
            wait_for(Check, Seconds - 1)
    end.

bucket_type_from(Collection) ->
    rj_util:any_to_binary(?RJ_TYPE(rj_util:any_to_list(Collection))).

bucket_with_type_from(Collection) ->
    {bucket_type_from(Collection), rj_util:any_to_binary(Collection)}.