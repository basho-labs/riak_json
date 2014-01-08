
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

-module(rj_wm_query).
-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    malformed_request/2,
    accept_json/2,
    to_json/2,
    is_authorized/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {
        collection,
        username,
        query_type,
        solr_query
    }).

%%% -- webmachine hooks

init(_) -> 
    {ok, #ctx{}}.

service_available(ReqData, Context=#ctx{}) ->
    {
        rj_config:is_enabled(),
        ReqData,
        Context
    }.

allowed_methods(ReqData, Context) ->
    {['PUT'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_json}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

malformed_request(ReqData, Context) ->
    case context_from(ReqData, Context) of
        {error, malformed_query} -> {true, ReqData, Context};
        PopulatedContext -> {false, ReqData, PopulatedContext}
    end.

accept_json(ReqData, Context) ->
    try
        {_Headers, JsonResults} = riak_json:find(Context#ctx.collection, Context#ctx.solr_query),
        lager:debug("Query: ~p~nResult: ~p~n", [Context#ctx.solr_query, JsonResults]),
        JsonResponse = rj_query_response:format_json_response(JsonResults, 
                                                              Context#ctx.query_type, 
                                                              Context#ctx.solr_query),
        {
            true,
            wrq:set_resp_body(JsonResponse, ReqData),
            Context
        }
    catch
        Exception:Cause ->
            lager:error("Query failed, ~p: ~p:~p~n~p", [Context#ctx.solr_query, Exception, Cause, erlang:get_stacktrace()]), 
            {{halt, 500}, ReqData, Context}
    end.


to_json(ReqData, Context) ->
    {
        true,
        ReqData,
        Context
     }.

is_authorized(ReqData, Context) ->
    case rj_auth_util:authorize(ReqData) of
        {ok, Username} ->
            rj_auth_util:success(ReqData, Context#ctx{username=Username});
        {failed, _Reason} ->
            rj_auth_util:failure(ReqData, Context);
        {error, _Reason} ->
            rj_auth_util:error(ReqData, Context)
    end.

%%% =================================================== internal functions

context_from(ReqData, Context) ->
    try
        Document = mochijson2:decode(wrq:req_body(ReqData)),
        Collection = wrq:path_info(collection, ReqData),
        QueryType = query_type_from(ReqData),
        assert_collection_has_index(Collection),
        SolrQuery = rj_query:from_json(Document, QueryType),

        Context#ctx{
            collection = Collection,
            query_type = QueryType,
            solr_query = SolrQuery
        }
    catch
        Exception:Reason ->
            lager:debug("Malformed query: ~p. ~p:~p", [wrq:req_body(ReqData), Exception, Reason]),
            {error, malformed_query}
    end.

query_type_from(ReqData) ->
    case wrq:disp_path(ReqData) of
        "all" -> all;
        "one" -> one;
        _ -> throw(unsupported_query_option)
    end.


assert_collection_has_index(Collection) ->
    case riak_json:index_exists(Collection) of
        undefined -> throw(collection_unindexed_or_doesnt_exist);
        _ -> Collection
    end.
