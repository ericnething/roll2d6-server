-module(couch_proxy).

-export([forward/3]).

forward(Path, Req, Opts) ->
    Method = cowboy_req:method(Req),
    ReqHeaders = maps:to_list(cowboy_req:headers(Req)),
    {ok, ReqBody, Req1} = read_body(Req, <<>>),

    io:format("Forwarding to CouchDB: ~p ~p ~p", [Method, Path, ReqHeaders]),
    Req2 = forward_to_couchdb(Method, Path, ReqHeaders, ReqBody, Req1),
    {ok, Req2, Opts}.

forward_to_couchdb(Method, Path, ReqHeaders, ReqBody, Req) ->
    {ok, ConnPid} = gun:open("localhost", 5984),
    {ok, http} = gun:await_up(ConnPid),
    StreamRef = gun:request(ConnPid, Method, Path, ReqHeaders, ReqBody),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Status, Headers} ->
            cowboy_req:reply(Status, maps:from_list(Headers), Req),
            no_data;
        {response, nofin, Status, Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            io:format("~p~n~p~n~p~n", [Status, Headers, Body]),
            cowboy_req:reply(Status, maps:from_list(Headers), Body, Req);
        Other ->
            io:format("Something else: ~p~n", [Other]),
            cowboy_req:reply(500, Req)
    end.

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.
