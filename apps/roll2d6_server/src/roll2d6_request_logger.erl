-module(roll2d6_request_logger).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    #{ peer := {IP, Port_},
       method := Method,
       path := Path
     } = Req,
    lager:info("~p ~p - ~p ~p", [ IP, Method, Path ]),
    {ok, Req, Env}.
