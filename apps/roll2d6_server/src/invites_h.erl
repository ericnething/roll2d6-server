-module(invites_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).

%% Custom callbacks.
-export([view_invite/2]).
-export([redeem_invite/2]).


init(Req, _State) ->
    {cowboy_rest, Req, _State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{content_type:json(), view_invite}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[{content_type:json(), redeem_invite}
     ], Req, State}.

is_authorized(Req0, State) ->
    util:is_authorized(Req0, State).


view_invite(Req0, State) ->
    {InviteCode, Req1} = cowboy_req:binding(invite_code, Req0),
    GameId = game_invite:get(InviteCode, Req1),
    Game = db:get_game(GameId),
    {jsx:encode(Game), Req1, State}.

redeem_invite(Req0, State = Jid) ->
    {InviteCode, Req1} = cowboy_req:binding(invite_code, Req0),
    GameId = game_invite:get(InviteCode, Req1),
    ok = db:add_player_to_game(Jid, GameId),
    {true, Req1, State}.
