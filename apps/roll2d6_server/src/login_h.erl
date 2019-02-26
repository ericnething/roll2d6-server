-module(login_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).

%% Custom callbacks.
-export([login_account/2]).

%% -record(credentials,
%%         { username :: string()
%%         , password :: string()
%%         }).
%% -type credentials() :: #credentials{}.


init(Req, _State) ->
    {cowboy_rest, Req, _State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{content_type:json(), login_account}
     ], Req, State}.

login_account(Req0, State) ->
    {Auth, Req1} = util:decode_body(Req0),
    {ok, Jid} = authenticate(Auth),
    SessionKey = session:new(Jid),
    Req2 = session:create_cookie(Req1, SessionKey),
    {true, Req2, State}.


%% -spec authenticate(credentials()) -> ok.

authenticate(Auth) -> 
    io:format("Authenticating ~p~n", [Auth]),
    #{ <<"username">> := Username
     , <<"password">> := Password
     } = Auth,
    % TODO: hash password
    HashedPassword = Password,
    Jid = db:get_person(Username, HashedPassword),
    {ok, Jid}.
