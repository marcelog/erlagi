#!/usr/bin/escript

-import(filename).
-import(escript).
-import(code).
-import(io).
-import(erlagi_demo).
-import(erlagi).

main(_) ->
    Path = escript:script_name(),
    Dir = filename:dirname(Path),
    true = code:add_patha(Dir ++ "/../compiled"),
    erlagi_fastagi:run(
        fun erlagi_demo:run/1,
        [
            { ip, "127.0.0.1" },
            { port, 4573 },
            { backlog, 10 },
            { spawn_server, false },
            { logfile, "/tmp/erlagi.log" }
        ]
    )
.

