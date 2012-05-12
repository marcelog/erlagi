#!/usr/bin/env escript

main(_) ->
    Path = escript:script_name(),
    Dir = filename:dirname(Path),
    true = code:add_patha(Dir ++ "/../ebin"),
    erlagi_fastagi:start_link(
        demo_app, [
            {host, "127.0.0.1"},
            {port, 20000},
            {backlog, 5},
            {callback, erlagi_demo},
            {logfile, "/tmp/erlagi.log"}
        ]
    ),
    receive
        _ -> ok
    end.

