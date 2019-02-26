-module(player_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([delete_resource/2]).

%% Custom callbacks.
-export([get_player/2]).
-export([update_player_access/2]).


-record(new_access, { new_access :: binary() }).

-type new_access() :: #new_access{}.

%%====================================================================
%% Cowboy REST callbacks
%%====================================================================

init(Req, _State) ->
    {cowboy_rest, Req, _State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{content_type:json(), get_player}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[{content_type:json(), update_player_access}
     ], Req, State}.

is_authorized(Req0, State) ->
    util:is_authorized(Req0, State).

forbidden(Req0, Jid) ->
    GameId = cowboy_req:binding(game_id, Req0),
    case db:get_game_access(Jid, GameId) of
        {ok, Access} -> {false, Req0, {Jid, GameId, Access}};
        forbidden -> {true, Req0, Jid}
    end.

%% DELETE: remove player from game
delete_resource(Req0, State = {Jid, GameId, Access}) ->
    PlayerId = cowboy_req:binding(player_id, Req0),
    PlayerAccess = db:get_player_access_for_game(PlayerId, GameId),
    CanRemove = can_remove_player(Jid, Access, PlayerId, PlayerAccess),
    case CanRemove of
        forbidden ->
            cowboy_req:reply(403, Req0),
            exit(normal);
        ok ->
            db:remove_player_from_game(PlayerId, GameId),
            {true, Req0, State}
    end.

%%====================================================================
%% Custom callbacks
%%====================================================================

%% GET: get information on a player in the game
get_player(Req0, State = {_Jid, GameId, _Access}) ->
    PlayerId = cowboy_req:binding(player_id, Req0),
    Player = db:get_player_for_game(PlayerId, GameId),
    {jsx:encode(Player), Req0, State}.


%% POST: update a player's game privileges
update_player_access(Req0, State = {Jid, GameId, RequesterAccess}) ->
    PlayerId = cowboy_req:binding(player_id, Req0),
    {NewAccess, Req1} = util:decode_body(Req0, fun from_json/1),
    PlayerAccess = db:get_player_access_for_game(PlayerId, GameId),
    CanUpdate = can_update_player(Jid, RequesterAccess, PlayerId, PlayerAccess),
    case CanUpdate of
        forbidden -> 
            cowboy_req:reply(403, Req1), 
            exit(normal);
        ok ->
            ok = db:update_player_access(PlayerId, GameId, NewAccess#new_access.new_access),
            {true, Req1, State}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% Rules for removing players

-spec can_remove_player(Jid, Access, PlayerId, PlayerAccess) -> Result when
      Jid          :: binary(), % id of the requester
      Access       :: binary(), % access privileges of the requester
      PlayerId     :: binary(), % id of the target player
      PlayerAccess :: binary(), % access privileges of the target player
      Result       :: ok | forbidden.

% A player may remove themself from a game
can_remove_player(_PlayerId, <<"player">>, _PlayerId, <<"player">>) -> ok;

% Players may not remove others from a game
can_remove_player(Jid, <<"player">>, PlayerId, _) 
  when Jid /= PlayerId -> forbidden;

% No one may remove the owner from a game
can_remove_player(_, _, _, <<"owner">>) -> forbidden;

% Owner and Game Masters may remove Game Masters or Players from a game
can_remove_player(_Jid, _, _PlayerId, _) -> ok.

%%--------------------------------------------------------------------
%% Rules for updating access privileges

-spec can_update_player(Jid, Access, PlayerId, PlayerAccess) -> Result when
      Jid          :: binary(), % id of the requester
      Access       :: binary(), % access privileges of the requester
      PlayerId     :: binary(), % id of the target player
      PlayerAccess :: binary(), % access privileges of the target player
      Result       :: ok | forbidden.

% Players cannot modify anyone's access privileges
can_update_player(_, <<"player">>, _, _) -> forbidden;

% No one may modify their own access privileges
can_update_player(_PlayerId, _, _PlayerId, _) -> forbidden;

% No one may modify the owner's access privileges
can_update_player(_, _, _, <<"owner">>) -> forbidden;

% Owners and Game Masters may modify privileges for other Game
% Masters and Players.
can_update_player(_Jid, _RequesterAccess, _PlayerId, _PlayerAccess) -> ok.

%%--------------------------------------------------------------------
%% Convert request body from JSON

-spec from_json(map()) -> new_access().

from_json(HashMap) ->
    #{<<"access">> := NewAccess} = HashMap,
    #new_access{ new_access = NewAccess }.
