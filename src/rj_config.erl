
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

-module(rj_config).

-export([
          is_enabled/0,
          document_name/0,
          infer_schemas/0,
          auth_bypass/0,
          auth_module/0,
          keystone_admin_username/0,
          keystone_admin_password/0,
          keystone_tenant/0,
          keystone_auth_url/0,
          keystone_token_url/0,
          keystone_user_url/0,
          keystone_permitted_roles/0
    ]).

is_enabled() ->
    case yokozuna:is_enabled(search) of
        true -> app_helper:get_env(riak_json, enabled, true);
        _ -> false
    end.

document_name() ->
    app_helper:get_env(riak_json, document_name, "document").

infer_schemas() ->
    app_helper:get_env(riak_json, infer_schemas, true).

%% internal
auth_section() ->
    app_helper:get_env(riak_json, auth,[]).

auth_bypass() ->
    proplists:get_value(bypass, auth_section(), true).

auth_module() ->
    proplists:get_value(module, auth_section(), undefined).

%% internal
auth_module_section(ModuleName) ->
    proplists:get_value(ModuleName, auth_section(), undefined).

%% internal
auth_module_variable(ModuleName, VariableName) ->
    case auth_module() of
        ModuleName ->
            proplists:get_value(VariableName, auth_module_section(ModuleName), undefined);
        _ ->
            {error, auth_module_not_defined}
    end.

%% Keystone specific
keystone_admin_username() ->
    auth_module_variable(rj_keystone_auth, admin_username).

keystone_admin_password() ->
    auth_module_variable(rj_keystone_auth, admin_password).

keystone_tenant() ->
    auth_module_variable(rj_keystone_auth, tenant).

keystone_auth_url() ->
    auth_module_variable(rj_keystone_auth, auth_url).

keystone_token_url() ->
    keystone_auth_url() ++ "/" ++ auth_module_variable(rj_keystone_auth, tokens_resource).

keystone_user_url() ->
    keystone_auth_url() ++ "/" ++ auth_module_variable(rj_keystone_auth, users_resource).

keystone_permitted_roles() ->
    sets:from_list(auth_module_variable(rj_keystone_auth, permitted_roles)).