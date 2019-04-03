-module(couch_proxy).

-export([forward/3]).

forward(Path, Req, Opts) ->
    Method = cowboy_req:method(Req),
    ReqHeaders = maps:to_list(cowboy_req:headers(Req)),
    {ok, ReqBody, Req1} = read_body(Req, <<>>),

    io:format("Forwarding to CouchDB: ~p ~p ~p~n", [Method, Path, ReqHeaders]),
    Req2 = forward_to_couchdb(Method, Path, ReqHeaders, ReqBody, Req1),
    {ok, Req2, Opts}.

forward_to_couchdb(Method, Path, ReqHeaders, ReqBody, Req0) ->
    Timeout = infinity,
    {ok, ConnPid} = gun:open("localhost", 5984),
    {ok, http} = gun:await_up(ConnPid, Timeout),
    StreamRef = gun:request(ConnPid, Method, Path, ReqHeaders, ReqBody),
    case gun:await(ConnPid, StreamRef, Timeout) of
        {response, fin, Status, Headers} ->
            io:format("~nJust the Headers~n~n"),
            cowboy_req:reply(Status, maps:from_list(Headers), Req0);
        {response, nofin, Status, RawHeaders} ->
            Headers = remove_non_http2_headers(RawHeaders),
            io:format("Stream Response: ~p~n~p~n", [Status, Headers]),
            Req = cowboy_req:stream_reply(Status, maps:from_list(Headers), Req0),
            stream_data(ConnPid, StreamRef, Timeout, Req);
        Other ->
            io:format("Something else: ~p~n", [Other]),
            cowboy_req:reply(500, Req0)
    end.

stream_data(ConnPid, StreamRef, Timeout, Req) ->
    case gun:await(ConnPid, StreamRef, Timeout) of
        {data, nofin, Data} ->
            io:format("Stream data: ~p~n", [Data]),
            cowboy_req:stream_body(Data, nofin, Req),
            stream_data(ConnPid, StreamRef, Timeout, Req);
        {data, fin, Data} ->
            io:format("Stream data final: ~p~n~n~p~n~n", [Data, Req]),
            cowboy_req:stream_body(Data, fin, Req),
            Req;
        Other ->
            io:format("Something else: ~p~n", [Other]),
            cowboy_req:reply(500, Req)
    end.

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

remove_non_http2_headers(Headers) ->
    lists:filter(fun({Name,_}) ->
      case Name of
          %% <<"connection">>        -> false;
          %% <<"transfer-encoding">> -> false;
          %% <<"accept-encoding">>   -> false;
          %% <<"content-encoding">>  -> false;
          %% <<"content-length">>    -> false;
          %% <<"keep-alive">>        -> false;
          %% <<"proxy-connection">>  -> false;
          %% <<"upgrade">>           -> false;
          _ -> true
      end
    end, Headers).

