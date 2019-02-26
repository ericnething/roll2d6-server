-module(register_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).

%% Custom callbacks.
-export([register_account/2]).

%% -record(credentials,
%%         { username :: string()
%%         , email    :: string()
%%         , password :: string()
%%         }).
%% -type credentials() :: #credentials{}.


init(Req, _State) ->
    {cowboy_rest, Req, _State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{content_type:json(), register_account}
     ], Req, State}.

register_account(Req0, State) ->
    {Auth, Req} = util:decode_body(Req0),
    create_account(Auth),
    {true, Req, State}.


%% -spec create_account(credentials()) -> ok.

create_account(Auth) -> 
    {ok, Jid} = create_jabber_account(Auth),
    ok = create_vtt_account(Jid, Auth),
    ok.


create_vtt_account(Jid, Auth) ->
    % hash password
    NewAuth = Auth,
    db:create_person(Jid, NewAuth).

create_jabber_account(Auth) ->
    io:format("Creating jabber account with ~p.~n", [Auth]),
    AuthJson = jsx:encode(maps:with([<<"username">>, <<"password">>], Auth)),
    {ok, ConnPid} = gun:open("localhost", 8088),
    {ok, _Protocol} = gun:await_up(ConnPid),
    StreamRef = gun:post(ConnPid, "/api/users/localhost",
                         [ {<<"content-type">>, "application/json"}
                         , {<<"accept">>, "application/json"}
                         ], 
                         AuthJson),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, _Status, _Headers} ->
            no_data;
        {response, nofin, Status, _Headers} ->
            {ok, _Body} = gun:await_body(ConnPid, StreamRef),
            io:format("~p~n", [Status]),
            case Status of
                Status_ when Status_ >= 200, Status_ < 300 ->
                    Username = string:lowercase(maps:get(<<"username">>, Auth)),
                    Jid = <<Username/binary, "@localhost">>,
                    {ok, Jid};
                _ ->
                    error
            end
    end.

