-module(sheet_id_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([is_authorized/2]).

%% Custom callbacks.
-export([get_sheet_id/2]).


init(Req, _State) ->
    {cowboy_rest, Req, _State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{content_type:json(), get_sheet_id}
     ], Req, State}.

is_authorized(Req0, State) ->
    util:is_authorized(Req0, State).


get_sheet_id(Req0, State) ->
    SheetId = esnowflake:generate_id(),
    {jsx:encode(erlang:integer_to_binary(SheetId)), Req0, State}.
