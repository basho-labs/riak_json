%% @doc Test the document API in various ways.
-module(rjt).

-compile(export_all).

-include("../../include/riak_json.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(integration_test).

test_bucket() ->
    "rjitest2".

test_bucket_type(Bucket) ->
    ?RJ_TYPE(Bucket).

fmt(S, Args) ->
    lists:flatten(io_lib:format(S, Args)).

use_auth() ->
    case os:getenv("USE_AUTH") of
        false -> false;
        "false" -> false;
        _ -> true
    end.

riak_host() ->
    case os:getenv("RIAK_HOST") of
        false -> "localhost";
        Host -> Host
    end.

riak_port() ->
    case os:getenv("RIAK_PORT") of
        false -> 8098;
        Port -> list_to_integer(Port)
    end.

url(Collection, Path) ->
    fmt("http://~s:~B/document/collection/~s~s", [riak_host(), riak_port(), Collection, Path]).
url(Path) ->
    url(test_bucket(), Path).

ensure_ibrowse() ->
    case whereis(ibrowse) of
        undefined -> ibrowse:start();
        Any when is_pid(Any)-> ok
    end.

auth_token() ->
    Obj = <<"{\"auth\": {\"tenantName\": \"riak_json\", \"passwordCredentials\": {\"username\": \"riak_json\", \"password\": \"riak_json\"}}}">>,
    {ok, "200", Resp} = http(post, "http://127.0.0.1:5000/v2.0/tokens", Obj, [], false),
    {struct,[{<<"access">>,{struct, AccessProps}}]} = mochijson2:decode(Resp),
    {struct, TokenProps} = proplists:get_value(<<"token">>, AccessProps),
    binary_to_list(proplists:get_value(<<"id">>, TokenProps)).

http(Method, URL, Body, H0, UseAuth, WithHeader) ->
    ensure_ibrowse(),
    Opts = [],
    H1 = case UseAuth of
        false -> H0;
        true -> H0 ++ [{"x-auth-token", auth_token()}]
    end,
    Headers = H1 ++ [
        {"content-type", "application/json"},
        {"accept", "application/json"}
    ],

    Res = ibrowse:send_req(URL, Headers, Method, Body, Opts),

    case WithHeader of
        true -> Res;
        _ ->
            {ok, S, _, B} = Res,
            {ok, S, B}
    end.

http(Method, URL) ->
    http(Method, URL, <<>>, [], use_auth(), false).
http(Method, URL, Body) ->
    http(Method, URL, Body, [], use_auth(), false).
http(Method, URL, Body, Headers) ->
    http(Method, URL, Body, Headers, use_auth(), false).
http(Method, URL, Body, Headers, UseAuth) ->
    http(Method, URL, Body, Headers, UseAuth, false).

clear(_, []) ->
    ok;
clear(Bucket, [Key|T]) ->
    KeyUrl = fmt("http://~s:~B/types/~s/buckets/~s/keys/~s", [riak_host(), riak_port(), test_bucket_type(Bucket), Bucket, Key]),
    {ok,_,_} = http(delete, KeyUrl, <<>>, [], false),
    clear(Bucket, T).

clear(Bucket) ->
    KeysUrl = fmt("http://~s:~B/types/~s/buckets/~s/keys?keys=true", [riak_host(), riak_port(), test_bucket_type(Bucket), Bucket]),
    PropsUrl = fmt("http://~s:~B/types/~s/buckets/~s/props", [riak_host(), riak_port(), test_bucket_type(Bucket), Bucket]),
    IndexUrl = fmt("http://~s:~B/yz/index/~s", [riak_host(), riak_port(), ?RJ_INDEX(Bucket)]),
    SchemaUrl = fmt("http://~s:~B/yz/schema/~s", [riak_host(), riak_port(), ?RJ_SCHEMA(Bucket)]),

    Keys = case http(get, KeysUrl, <<>>, [], false) of
        {ok,"200",Resp} ->
            {struct,[{<<"keys">>,K}]} = mochijson2:decode(Resp),
            K;
        _ -> []
    end,

    ?debugFmt("Keys getting deleted from ~p: ~p", [Bucket, Keys]),
    clear(Bucket, Keys),
    http(delete, PropsUrl, <<>>, [], false),
    http(delete, IndexUrl, <<>>, [], false),
    http(delete, SchemaUrl, <<>>, [], false),
    timer:sleep(2000).

clear() ->
    clear(test_bucket()).

-endif.
