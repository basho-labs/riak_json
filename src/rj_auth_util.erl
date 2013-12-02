
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

-module(rj_auth_util).

-export([authorize/1, failure/2, error/2, success/2]).

authorize(ReqData) ->
    maybe_authorize(ReqData, rj_config:auth_bypass()).

% Internal
maybe_authorize(ReqData, false) ->
    erlang:apply(rj_config:auth_module(), authorize, [ReqData]);
maybe_authorize(_, _) ->
    %% Anonymous attempt
    {ok, <<"Anonymous">>}.

failure(ReqData, Context) ->
    lager:warning("Failed login attempt, request data: ~p", [ReqData]),
    {{halt, 403}, wrq:set_resp_header(
                            "Content-type", "text/plain",
                            wrq:set_resp_body("NOT AUTHORIZED", ReqData)
                        ), Context}.

error(ReqData, Context) ->
    lager:warning("Error on login attempt, request data: ~p", [ReqData]),
    {{halt, 401}, wrq:set_resp_header(
                            "Content-type", "text/plain",
                            wrq:set_resp_body("NOT AUTHORIZED", ReqData)
                        ), Context}.

success(ReqData, Context) ->
    {true, ReqData, Context}.