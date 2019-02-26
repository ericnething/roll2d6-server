-module(couch_proxy_h).
-behavior(cowboy_handler).

-export([init/2]).

init(Req, Opts) ->
    couch_proxy:forward(<<"/">>, Req, Opts).
