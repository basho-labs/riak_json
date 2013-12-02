
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

-module(rj_keystone_auth).

-export([identify/1, authorize/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% ===================================================================
%% Public API
%% ===================================================================

identify(RequestData) ->
	AuthToken = wrq:get_req_header("x-auth-token", RequestData),
	if AuthToken == undefined ->
			{error, unable_to_identify};
		true ->
			case request_keystone_token_info(AuthToken) of 
				{error, _} -> 
					{error, failed_to_identify};
				{_Token, Username, _UserId, Roles} -> 
					{ok, Username, Roles}
			end
	end.
	

authorize(RequestData) ->
	PermittedRoles = rj_config:keystone_permitted_roles(),

	case identify(RequestData) of
		{ok, Username, Roles} ->
			Authorized = not sets:is_disjoint(Roles, PermittedRoles),
			if Authorized == true ->
					{ok, Username};
				true ->
					{failed, user_not_permitted}
			end;
		{error, failed_to_identify} ->
			{failed, bad_token};
		{error, unable_to_identify} -> 
			{error, token_not_found}
	end.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @todo Cache token or figure out how to arbitrarily assign a token
%% in keystone 
retrieve_admin_token() ->
	RequestURI = rj_config:keystone_token_url(),
	Username = rj_config:keystone_admin_username(),
	Password = rj_config:keystone_admin_password(),
	TenantName = rj_config:keystone_tenant(),
	retrieve_token(RequestURI, Username, Password, TenantName).

retrieve_token(RequestURI, Username, Password, TenantName) ->
	Type = "application/json",
	BodyJson = {struct,[{<<"auth">>,
          			{struct,[{<<"tenantName">>,TenantName},
          					 {<<"passwordCredentials">>,
                    			{struct,[{<<"username">>,Username},
                         	     		 {<<"password">>,Password}]}
             	     		  }]}
                    	}
                ]},
	Body = iolist_to_binary(mochijson2:encode(BodyJson)),
	RequestHeaders = [{"Content-Length", byte_size(Body)}],

	Response = httpc:request(post, {RequestURI, RequestHeaders, Type, Body}, [], []),
	handle_token_info_response(Response).
	
request_keystone_token_info(AuthToken) ->
	AuthURI = rj_config:keystone_token_url(),
	{AdminToken, _, _, _} = retrieve_admin_token(),
	request_keystone_token_info(AuthURI, AuthToken, binary_to_list(AdminToken)).

request_keystone_token_info(AuthURI, AuthToken, AdminToken) ->
	RequestURI = AuthURI ++ "/" ++ AuthToken,

	RequestHeaders = [{"X-Auth-Token", AdminToken}],

	Response = httpc:request(get, {RequestURI, RequestHeaders}, [], []),

	handle_token_info_response(Response).

handle_token_info_response({ok, {{_HTTPVer, _Status, _StatusLine}, _, TokenInfo}})
  when _Status >= 200, _Status =< 299 ->
    DecodedJson = decode_token_response(TokenInfo),
    
    extract_user_data(DecodedJson);
handle_token_info_response({ok, {{_HTTPVer, _Status, _StatusLine}, _, _}}) ->
    {error, failed_to_validate};
handle_token_info_response({error, Reason}) ->
    _ = lager:warning("Error occurred requesting token from keystone. Reason: ~p",
                  [Reason]),
    {error, failed_to_validate}.

decode_token_response(JsonString) ->
	{struct, TokenResponse} = from_json(JsonString),
	TokenResponse.

from_json(JsonString) ->
	case catch mochijson2:decode(JsonString) of
		{'Exit', _} ->
			{error, decode_failed};
		Result ->
			Result
	end.

extract_user_data(Json) ->
	% do {error, Reason} if value is not found
	{struct, Access} = proplists:get_value(<<"access">>, Json, {error, not_found}),
	{struct, Token} = proplists:get_value(<<"token">>, Access, {error, not_found}),
	TokenId = proplists:get_value(<<"id">>, Token, {error, not_found}),
	{struct, User} = proplists:get_value(<<"user">>, Access, {error, not_found}),
	UserName = proplists:get_value(<<"name">>, User, {error, not_found}),
	UserId = proplists:get_value(<<"id">>, User, {error, not_found}),
	% Name = proplists:get_value(User, <<"name">>, {error, not_found}),
	Roles = extract_roles(User),

	{TokenId, UserName, UserId, Roles}.

extract_roles(UserData) ->
	RoleDicts = proplists:get_value(<<"roles">>, UserData),
	extract_roles(RoleDicts, []).
extract_roles([], Acc) ->
	sets:from_list(Acc);
extract_roles([{struct, Role}|Tail], Acc) ->
	RoleName = proplists:get_value(<<"name">>, Role, {error, not_found}),
	extract_roles(Tail, [RoleName | Acc]).

-ifdef(TEST).

% live_keystone_test() ->
% 	inets:start(),

% 	AdminUsername = <<"riak_json_admin">>,
% 	AdminPassword = <<"riak_json_admin">>,
% 	Username = <<"riak_json">>,
% 	Password = <<"riak_json">>,
% 	Tenant = <<"riak_json">>,

% 	AuthURI = "http://localhost:5000/v2.0/tokens",

% 	{AdminToken, RetAdminUsername, _UserId, _Roles} = retrieve_token(AuthURI, AdminUsername, AdminPassword, Tenant),
% 	?assertEqual(AdminUsername, RetAdminUsername),

% 	{UserToken, RetUsername, _UserId2, _Roles2} = retrieve_token(AuthURI, Username, Password, Tenant),
% 	?assertEqual(Username, RetUsername),

% 	lager:info("User Token: ~p~nAdmin Token: ~p~n", [UserToken, AdminToken]),

% 	Response = request_keystone_token_info(AuthURI, 
% 										   binary_to_list(UserToken), 
% 										   binary_to_list(AdminToken)),

% 	{_ValidatedToken, ValidatedUsername, _UserId3, ValidatedRoles} = Response,
% 	?assertEqual(Username, ValidatedUsername),
% 	?assertNot(sets:is_disjoint(ValidatedRoles, sets:from_list([Tenant]))),

% 	inets:stop().

user_data_test() ->
	Token = "{\"access\":{\"token\":{\"expires\":\"2012-02-05T00:00:00\","
            "\"id\":\"887665443383838\", \"tenant\":{\"id\":\"1\", \"name\""
            ":\"customer-x\"}}, \"user\":{\"name\":\"joeuser\", \"tenantName\""
            ":\"customer-x\", \"id\":\"1\", \"roles\":[{\"serviceId\":\"1\","
            "\"id\":\"3\", \"name\":\"Member\"}], \"tenantId\":\"1\"}}}",

    {ValidTokenId, ValidUserName, ValidUserId, Roles} = extract_user_data(decode_token_response(Token)),   
    ?assertEqual(<<"887665443383838">>, ValidTokenId),
    ?assertEqual(<<"joeuser">>, ValidUserName),
    ?assertEqual(<<"1">>, ValidUserId),
    ?assertNot(sets:is_disjoint(Roles, sets:from_list([<<"Member">>]))).

-endif.