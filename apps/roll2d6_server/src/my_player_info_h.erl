-module(my_player_info_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([forbidden/2]).

%% Custom callbacks.
-export([get_my_player_info/2]).


%%====================================================================
%% Cowboy REST callbacks
%%====================================================================

init(Req, _State) ->
    {cowboy_rest, Req, _State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{content_type:json(), get_my_player_info}
     ], Req, State}.

is_authorized(Req0, State) ->
    util:is_authorized(Req0, State).

forbidden(Req0, Jid) ->
    GameId = cowboy_req:binding(game_id, Req0),
    case db:get_game_access(Jid, GameId) of
        {ok, Access} -> {false, Req0, {Jid, GameId, Access}};
        forbidden -> {true, Req0, Jid}
    end.

%%====================================================================
%% Custom callbacks
%%====================================================================

%% GET: get information on a player in the game
get_my_player_info(Req0, State = {Jid, GameId, _Access}) ->
    Player = db:get_player_for_game(Jid, GameId),
    {jsx:encode(Player), Req0, State}.

