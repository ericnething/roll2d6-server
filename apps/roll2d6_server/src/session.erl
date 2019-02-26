-module(session).
-export([ new/1
        , get/1
        , delete/1
        , create_cookie/2
        , delete_cookie/1
        ]).

-define(SESSION_PREFIX, <<"session:">>).
-define(EXPIRATION, 3600).

-spec delete(Req) -> ok when
      Req :: cowboy_req:req().

delete(Req0) ->
    try get_cookie(Req0) of
        {ok, SessionKey} ->
            Key = [?SESSION_PREFIX, SessionKey],
            {ok, _} = eredis:q(redis_conn, ["DEL", Key]),
            ok
    catch
        _:_ -> cowboy_req:reply(500 ,Req0), % unauthorized
               exit(normal)
    end.


-spec get(Req) -> Jid when
      Req :: cowboy_req:req(),
      Jid :: binary().

get(Req0) ->
    try get_cookie(Req0) of
        {ok, SessionKey} ->
            Key = [?SESSION_PREFIX, SessionKey],
            case eredis:q(redis_conn, ["GET", Key]) of
                {ok, undefined} ->
                    cowboy_req:reply(401, Req0), % unauthorized
                    exit(normal);

                {ok, Id} -> {ok, Id};

                {error, _} ->
                    cowboy_req:reply(500, Req0), % server error
                    exit(normal)
            end
    catch
        _:_ -> 
            cowboy_req:reply(401, Req0), % unauthorized
            exit(normal)
    end.

-spec new(Jid) -> SessionKey when
      Jid :: binary(),
      SessionKey :: binary().

new(Jid) ->
    SessionKey = base64:encode(crypto:strong_rand_bytes(32)),
    Key = [?SESSION_PREFIX, SessionKey],
    eredis:qp(redis_conn, [ ["SET", Key, Jid]
                          , ["EXPIRE", Key, ?EXPIRATION]
                          ]),
    SessionKey.


%%% ----- Cookies -----

create_cookie(Req, SessionKey) ->
    set_cookie(Req, SessionKey, ?EXPIRATION).

delete_cookie(Req) ->
    set_cookie(Req, <<>>, 0).

-spec set_cookie(Req, SessionKey, Expiration) -> Req when
      Req :: cowboy_req:req(),
      SessionKey :: binary(),
      Expiration :: integer().

set_cookie(Req0, SessionKey, Expiration) ->
    cowboy_req:set_resp_cookie(
      <<"session">>, SessionKey,
      Req0, #{ http_only => true
             %% , secure => true
             , path => "/"
             , max_age => Expiration
             }).

get_cookie(Req0) ->
    #{session := SessionKey} = 
        cowboy_req:match_cookies([{session, nonempty}], Req0),
    {ok, SessionKey}.
