-module(esegment_http).

-include("segment.hrl").

-define(BASE_URL, "https://api.segment.io/v1/").

%% API exports
-export([post/3]).

%%====================================================================
%% API functions
%%====================================================================
-spec post(Method, Object, #segment{}) -> {ok, Resp} | {error, Reason} when
    Method :: atom(),
    Object :: track() | identify() | page() | screen() | alias() | group(),
    Resp :: map(),
    Reason :: term().
post(Method, Object, WriteKey) ->
    case httpc:request(post, request(method(Method), WriteKey, Object), [], []) of
        {ok,{{_ ,200,_}, _Headers, Body }} ->
            {ok, jiffy:decode(Body, [return_maps])};
        Non200Resp ->
            {error, Non200Resp}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
url(Method) ->
    ?BASE_URL ++ Method.


headers(WriteKey) ->
    [
        {"Authorization", "Basic " ++ base64:encode_to_string(WriteKey)},
        {"accept", "application/json"}
    ].

request(Method, WriteKey, Object) ->
    {url(Method), headers(WriteKey), {"Content-Type", "application/json"}, jiffy:encode(Object)}.

method(track) ->
    "track";
method(identify) ->
    "identify";
method(alias) ->
    "alias";
method(page) ->
    "page";
method(screen) ->
    "screen";
method(group) ->
    "group".
