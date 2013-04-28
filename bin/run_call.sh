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
    erlagi_demo:new_call(erlagi:new_call()).

