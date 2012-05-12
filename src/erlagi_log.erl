%%% Own logging functions.
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
-module(erlagi_log).

-include("erlagi_types.hrl").

-export([open/1, log/2, log/3, close/1, get_logger/1]).

open(Filename) ->
    {ok, IoDevice} = file:open(Filename, [append]),
    IoDevice.

log(#agilog{} = Log, Format, Data) when is_list(Data) ->
    io:format(Log#agilog.iodevice, Format, Data).

log(#agilog{} = Log, Format) ->
    log(Log, Format, []).

close(#agilog{iodevice = Iodevice}) ->
    ok = file:close(Iodevice).

get_logger(Filename) ->
    Dev = open(Filename),
    #agilog{filename=Filename, iodevice=Dev}.

