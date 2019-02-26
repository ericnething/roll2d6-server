-module(logout_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).

%% Custom callbacks.
-export([logout_account/2]).


init(Req, _State) ->
    {cowboy_rest, Req, _State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{content_type:json(), logout_account}
     ], Req, State}.

is_authorized(Req0, State) ->
    util:is_authorized(Req0, State).

logout_account(Req0, State) ->
    ok = session:delete(Req0),
    Req1 = session:delete_cookie(Req0),
    {true, Req1, State}.
