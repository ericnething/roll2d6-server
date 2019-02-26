-module(content_type).
-export([json/0]).

json() ->
    {<<"application">>, <<"json">>, '*'}.
