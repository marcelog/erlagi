-module(erlagi_log).

-include("erlagi_types.hrl").
-import(io).
-import(file).
-import(erlagi_misc).

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

