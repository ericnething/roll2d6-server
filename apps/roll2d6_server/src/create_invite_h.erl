-module(create_invite_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).

%% Custom callbacks.
-export([create_invite/2]).


init(Req, _State) ->
    {cowboy_rest, Req, _State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{content_type:json(), create_invite}
     ], Req, State}.

is_authorized(Req0, State) ->
    util:is_authorized(Req0, State).


create_invite(Req0, State) ->
    GameId = cowboy_req:binding(game_id, Req0),

    Expiration = 86400, % one day
    InviteCode = game_invite:new(GameId, Expiration),
    Req1 = cowboy_req:reply(
             201,
             #{<<"content-type">> => <<"application/json">>},
             to_json(InviteCode),
             Req0),
    {stop, Req1, State}.


%%% --- Utilities ---

to_json(InviteCode)->
    Data = #{ <<"inviteCode">> => InviteCode },
    jsx:encode(Data).

