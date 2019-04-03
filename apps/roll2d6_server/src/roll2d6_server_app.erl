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

    RestApiDispatch = cowboy_router:compile([
        {'_', [ {"/register", register_h, []}
              , {"/login", login_h, []}
              , {"/logout", logout_h, []}
              , {"/games", games_h, []}
              , {"/games/:game_id/invites", create_invite_h, []}
              , {"/invites/:invite_code", invites_h, []}
              , {"/sheet-id", sheet_id_h, []}
              , {"/games/:game_id/players", players_h, []}
              , {"/games/:game_id/players/:player_id", player_h, []}
              , {"/games/:game_id/my-player-info", my_player_info_h, []}
              ]}
    ]),
    CouchProxyDispatch = cowboy_router:compile([
        {'_', [ {"/couchdb", couch_proxy_h, []}
              , {"/couchdb/:game_id/[...]", couch_proxy_game_h, []}
              ]}
    ]),
    {ok, _} = cowboy:start_clear(rest_api, [{port, 3000}], #{ 
        env => #{dispatch => RestApiDispatch },
        metrics_callback => fun show_metrics/1,
        stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
    }),
    {ok, _} = cowboy:start_clear(couch_proxy, [{port, 3001}], #{ 
        env => #{dispatch => CouchProxyDispatch },
        idle_timeout => infinity,
        metrics_callback => fun show_metrics/1,
        stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
    }),
    roll2d6_server_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

show_metrics(Metrics) ->
    Req = maps:get(req, Metrics),
    #{ peer := {IP, Port_},
       method := Method,
       path := Path,
       version := Version,
       qs := QS
     } = Req,
    lager:log(info, self(), "~s ~s ~s ~s ~n", [Version, Method, Path, QS]).
