-module(players_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([forbidden/2]).

%% Custom callbacks.
-export([get_players/2]).
-export([add_player/2]).


init(Req, _State) ->
    {cowboy_rest, Req, _State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{content_type:json(), get_players}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[{content_type:json(), add_player}
     ], Req, State}.

is_authorized(Req0, State) ->
    util:is_authorized(Req0, State).

forbidden(Req0, Jid) ->
    GameId = cowboy_req:binding(game_id, Req0),
    case db:get_game_access(Jid, GameId) of
        {ok, Access} -> {false, Req0, {Jid, GameId, Access}};
        forbidden -> {true, Req0, Jid}
    end.


get_players(Req0, State = {_Jid, GameId, _Access}) ->
    Players = db:get_players(GameId),
    {jsx:encode(Players), Req0, State}.

add_player(Req0, State = {_Jid, GameId, Access}) ->
    ok = check_access(Req0, Access),
    {Body, Req1} = util:decode_body(Req0),
    #{<<"id">> := NewPlayerJid} = Body,
    ok = db:add_player_to_game(NewPlayerJid, GameId),
    {true, Req1, State}.


check_access(Req0, Access) ->
    case Access of
        <<"owner">> -> ok;
        <<"game_master">> -> ok;
        <<"player">> -> cowboy_req:reply(403, Req0),
                        exit(normal)
    end.
