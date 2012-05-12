#!/usr/bin/env escript

main(_) ->
    Path = escript:script_name(),
    Dir = filename:dirname(Path),
    true = code:add_patha(Dir ++ "/../ebin"),
    erlagi_demo:new_call(erlagi:new_call(
        erlagi_log:get_logger("/tmp/erlagi.log")
    )).

