%%% Returns output functions for stdin/stdout AGI connections.
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
-module(erlagi_io_normal).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-export([get_close_fun/0, get_send_fun/0, get_recv_fun/0]).

get_close_fun() ->
    fun() ->
        ok
    end.

get_send_fun() ->
    fun(Text) ->
        io:format("~s", [Text])
    end.

get_recv_fun() ->
    fun() ->
        io:get_line(standard_io, "")
    end.

