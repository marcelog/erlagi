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

-export([start_link/2, loop/2]).

start_link(Socket, Callback) ->
    {ok, spawn_link(?MODULE, loop, [Socket, Callback])}.

loop(Socket, Callback) ->
    AcceptResult = gen_tcp:accept(Socket, 5),
    case AcceptResult of
        {ok, ClientSocket} ->
            Call = erlagi:new_call(
                erlagi_io_tcp:get_recv_fun(ClientSocket),
                erlagi_io_tcp:get_send_fun(ClientSocket),
                erlagi_io_tcp:get_close_fun(ClientSocket)
            ),
            Handler = spawn(
                fun() ->
                    receive
                        go ->
                            apply(Callback, new_call, [Call])
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
            lager:info("Terminated by user"),
            gen_tcp:close(Socket),
            exit(terminated_by_user)
    after 5 ->
        true
    end,
    loop(Socket, Callback).

