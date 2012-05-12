%%% Returns output functions for FastAGI (tcp) connections.
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
-module(erlagi_io_tcp).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-export([get_close_fun/1, get_send_fun/1, get_recv_fun/1]).

get_close_fun(Socket) ->
    fun() ->
        gen_tcp:close(Socket)
    end.

get_send_fun(Socket) ->
    fun(Text) ->
        ok = gen_tcp:send(Socket, Text)
    end.

get_recv_fun(Socket) ->
    fun() ->
        {ok, Text} = gen_tcp:recv(Socket, 0),
        Text
    end.

