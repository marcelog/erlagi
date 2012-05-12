%%% This one is in charge of accepting a connection and handle
%%% it.
%%%
%%% Copyright 2012 Marcelo Gornstein <marcelog@gmail.com>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
-module(erlagi_fastagi_worker).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-export([start_link/3, loop/3]).

start_link(Socket, Log, Callback) ->
    {ok, spawn_link(?MODULE, loop, [Socket, Log, Callback])}.

loop(Socket, Log, Callback) ->
    AcceptResult = gen_tcp:accept(Socket, 5),
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
                            Callback(Call),
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
    after 5 ->
        true
    end,
    loop(Socket, Log, Callback).

