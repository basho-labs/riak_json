
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

-module(rj_wm_document).
-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    allow_missing_post/2,
    content_types_accepted/2,
    content_types_provided/2,
    malformed_request/2,
    resource_exists/2,
    post_is_create/2,
    create_path/2,
    process_post/2,
    accept_json/2,
    delete_resource/2,
    to_json/2,
    is_authorized/2
    ]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {
    collection,
    key,
    doc,
    username
    }).

%%% -- webmachine hooks 

init(_) ->
    {ok, #ctx{}}.

service_available(ReqData, Context=#ctx{}) ->
    {
        rj_config:is_enabled(),
        ReqData,
        Context#ctx{
            collection = wrq:path_info(collection, ReqData),
            key = wrq:path_info(key, ReqData)
        }
    }.

allowed_methods(ReqData, Context=#ctx{key=undefined}) ->
    {['POST'], ReqData, Context};
allowed_methods(ReqData, Context) ->
    {['GET', 'POST', 'PUT', 'DELETE'], ReqData, Context}.

allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_json}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.    

malformed_request(ReqData, Context=#ctx{collection=undefined}) ->
    {true, ReqData, Context};
malformed_request(ReqData, Context) ->
    {false, ReqData, Context}.

resource_exists(ReqData, Context) ->
    C1 = ensure_doc(Context),
    case C1#ctx.doc of
        undefined -> {false, ReqData, C1};
        _ -> {true, ReqData, C1}
    end.

post_is_create(ReqData, Context=#ctx{key=undefined}) ->
    {true, ReqData, Context};
post_is_create(ReqData, Context) ->
    {false, ReqData, Context}.

create_path(ReqData, Context) ->
    K = riak_core_util:unique_id_62(),
    {K,
     wrq:set_resp_header("Location",
                         string:join([rj_config:document_name(), "collection", Context#ctx.collection, K], "/"),
                         ReqData),
     Context#ctx{key=K}}.

process_post(ReqData, Context) ->
    store_document(ReqData, Context).

accept_json(ReqData, Context) ->
    store_document(ReqData, Context).

delete_resource(ReqData, Context) ->
    delete_document(ReqData, Context).

to_json(ReqData, Context) ->
    {Context#ctx.doc, ReqData, Context}.

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

ensure_doc(Context) when Context#ctx.key =:= undefined ->
    Context;
ensure_doc(Context) ->
    case riak_json:get_document(Context#ctx.collection, Context#ctx.key) of
        undefined -> Context;
        V -> Context#ctx{doc = V}
    end.

store_document(ReqData, Context) ->
    JDocument = wrq:req_body(ReqData),
    Response = riak_json:store_document(Context#ctx.collection, Context#ctx.key, JDocument),

    case Response of
        ok ->  {true, ReqData, Context};
        Other -> Other
    end.

delete_document(ReqData, Context) ->
    Response = riak_json:delete_document(Context#ctx.collection, Context#ctx.key),

    case Response of
        ok ->
            {true, ReqData, Context};
        _ ->
            {false, ReqData, Context}
    end.