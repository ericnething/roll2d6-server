-module(games_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).

%% Custom callbacks.
-export([get_games/2]).
-export([create_game/2]).


init(Req, _State) ->
    {cowboy_rest, Req, _State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{content_type:json(), get_games}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[{content_type:json(), create_game}
     ], Req, State}.

is_authorized(Req0, State) ->
    util:is_authorized(Req0, State).


get_games(Req0, Jid) ->
    Games = db:get_games_for_player(Jid),
    {jsx:encode(Games), Req0, Jid}.


create_game(Req0, Jid) ->
    {NewGame, Req1} = util:decode_body(Req0),
    #{ <<"title">>    := Title
     , <<"gameType">> := GameType
     } = NewGame,
    
    % generate game id
    GameId = esnowflake:generate_id(),

    ok = db:create_new_game(Jid, GameId, Title, GameType),
    {true, Req1, Jid}.

