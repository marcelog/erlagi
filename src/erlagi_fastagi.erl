-module(erlagi_fastagi).

-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-import(erlagi).
-import(erlagi_log).
-import(erlagi_io_tcp).
-import(erlagi_options).
-import(erlagi_defaults).
-import(inet_parse).
-import(gen_tcp).

-export([run/2, run/1]).

accept_loop(Socket, Log, CallHandler) ->
    AcceptResult = gen_tcp:accept(Socket, 10),
    case AcceptResult of
        {ok, ClientSocket} ->
            Call = erlagi:new_call(
                Log,
                erlagi_io_tcp:get_recv_fun(ClientSocket),
                erlagi_io_tcp:get_send_fun(ClientSocket),
                erlagi_io_tcp:get_close_fun(ClientSocket)
            ),
            Handler = spawn(
                fun() ->
                    receive
                        go ->
                            CallHandler(Call),
                            erlang:exit(call_done)
                    after 5000 ->
                        erlang:error(never_got_go)
                    end
                end
            ),
            gen_tcp:controlling_process(ClientSocket, Handler),
            Handler ! go;
        {error, timeout} -> true
    end,
    receive
        close ->
            erlagi_log:log(Log, "Terminated by user~n"),
            gen_tcp:close(Socket),
            exit(terminated_by_user)
    after 10 ->
        true
    end,
    accept_loop(Socket, Log, CallHandler)
.

open_socket(Config) when is_list(Config) ->
    { ok, Ip } = inet_parse:address(erlagi_options:get_option(ip, Config)),
    Port = erlagi_options:get_option(port, Config),
    Backlog = erlagi_options:get_option(backlog, Config),
    Options = [
        { ip, Ip},
        { active, false },
        { reuseaddr, true },
        { backlog, Backlog }
    ],
    {ok, Socket} = gen_tcp:listen(Port, Options),
    Socket
.

run(CallHandler, Config) when is_list(Config) ->
    Socket = open_socket(Config),
    Spawn = erlagi_options:get_option(spawn_server, Config),
    Log = erlagi_log:get_logger(erlagi_options:get_option(logfile, Config)),
    case Spawn of
        true -> 
            Pid = spawn(fun() ->
                receive
                    go ->
                        erlagi_log:log(Log, "Spawned FastAGI server~n"),
                        accept_loop(Socket, Log, CallHandler)
                after 5000 ->
                    erlang:error(never_got_go)
                end
            end),
            gen_tcp:controlling_process(Socket, Pid),
            Pid ! go,
            Pid;
        false -> accept_loop(Socket, Log, CallHandler)
    end
.

run(CallHandler) ->
    run(CallHandler, erlagi_defaults:get_default_options())
.

