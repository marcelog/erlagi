-module(erlagi_defaults).

-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-export( [ get_default_options/0 ] ).

get_default_options() ->
    [
        { ip, "127.0.0.1" },
        { port, 4573 },
        { backlog, 10 },
        { logfile, "/tmp/erlagi.log" },
        { spawn_server, true }
    ].


