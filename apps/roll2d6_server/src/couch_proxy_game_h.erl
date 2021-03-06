-module(couch_proxy_game_h).
-behavior(cowboy_handler).

-export([init/2]).

init(Req, Opts) ->
    GameId = cowboy_req:binding(game_id, Req),
    % check credentials here

    <<"/couchdb", Path/binary>> = cowboy_req:path(Req),
    QS = cowboy_req:qs(Req),
    couch_proxy:forward([Path, "?", QS], Req, Opts).
