#!/usr/bin/escript

-import(io).
-import(code).
-import(escript).
-import(filename).
-import(erlagi).
-import(erlagi_log).
-import(erlagi_demo).

main(_) ->
    Path = escript:script_name(),
    Dir = filename:dirname(Path),
    true = code:add_patha(Dir ++ "/../ebin"),
    erlagi_demo:run(erlagi:new_call(erlagi_log:get_logger("/tmp/erlagi.log")))
.

