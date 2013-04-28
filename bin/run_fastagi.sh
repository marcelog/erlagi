#!/usr/bin/env escript

main(_) ->
    Path = escript:script_name(),
    Dir = filename:dirname(Path),
    true = code:add_patha(Dir ++ "/../ebin"),
    true = code:add_patha(Dir ++ "/../deps/lager/ebin"),
    ok = application:start(sasl),
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(lager),
    erlagi_fastagi:start_link(
        demo_app, [
            {host, "127.0.0.1"},
            {port, 4573},
            {backlog, 5},
            {callback, erlagi_demo}
        ]
    ),
    receive
        _ -> ok
    end.

