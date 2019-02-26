-module(game_invite).

-export([ get/2
        , new/2
        ]).

-define(INVITE_PREFIX, <<"game_invite:">>).


get(InviteCode, Req) ->
    Key = make_key(InviteCode),
    case eredis:q(redis_conn, ["GET", Key]) of
        {ok, undefined} ->
            cowboy_req:reply(404, Req),
            exit(normal);
        {ok, GameId} -> GameId
    end.

new(GameId, Expiration) ->
    InviteCode = generate_new_invite(),
    Key = make_key(InviteCode),
    eredis:qp(redis_conn, [ ["SET", Key, GameId]
                          , ["EXPIRE", Key, Expiration]]),
    InviteCode.

generate_new_invite() ->
    base64:encode(crypto:strong_rand_bytes(32)).

make_key(InviteCode) ->
    [?INVITE_PREFIX, InviteCode].
