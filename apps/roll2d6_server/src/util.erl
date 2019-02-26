-module(util).

-export([ decode_body/1
        , decode_body/2
        , is_authorized/2
        ]).

decode_body(Req0) ->
    Identity = fun (A) -> A end,
    decode_body_internal(Req0, Identity).

decode_body(Req0, ToRecord) ->
    decode_body_internal(Req0, ToRecord).

decode_body_internal(Req0, ToRecord) ->
    {ok, Value, Req} = cowboy_req:read_body(Req0),
    HashMap = jsx:decode(Value, [return_maps]),
    {ToRecord(HashMap), Req}.

is_authorized(Req0, State) ->
    case session:get(Req0) of
        {ok, Jid} -> {true, Req0, Jid};
        _ -> {{false, <<"Bearer realm=\"cowboy\"">>}, Req0, State}
    end.
