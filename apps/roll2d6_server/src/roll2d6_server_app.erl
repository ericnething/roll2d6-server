%%%-------------------------------------------------------------------
%% @doc roll2d6_server public API
%% @end
%%%-------------------------------------------------------------------

-module(roll2d6_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    % create Redis pool
    {ok, RedisConn} = eredis:start_link(),
    register(redis_conn, RedisConn),

    Dispatch = cowboy_router:compile([
        {'_', [ {"/register", register_h, []}
              , {"/login", login_h, []}
              , {"/logout", logout_h, []}
              , {"/games", games_h, []}
              , {"/games/:game_id/invites", create_invite_h, []}
              , {"/invites/:invite_code", invites_h, []}
              , {"/sheet-id", sheet_id_h, []}
              , {"/games/:game_id/players", players_h, []}
              , {"/games/:game_id/players/:player_id", player_h, []}
              , {"/couchdb", couch_proxy_h, []}
              , {"/couchdb/:game_id/[...]", couch_proxy_game_h, []}
              ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{ 
        env => #{dispatch => Dispatch}
    }),
    roll2d6_server_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
