-module(erlagi).

-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-include("erlagi_types.hrl").

-import(io).
-import(lists).
-import(string).
-import(erlagio_io).
-import(erlagio_log).
-import(erlagio_io_normal).
-import(erlagi_misc).
-import(erlagi_result).
-import(erlagi_read_env).

-export( [
    new_call/4, new_call/1, terminate/1,
    answer/1, hangup/1, stream_file/3, stream_file/2, exec/2, exec/3, log/3, verbose/2,
    log_notice/2, log_debug/2, log_warn/2, log_error/2, log_verbose/2, log_dtmf/2,
    stop_play_tones/1, play_custom_tones/2, play_tone/2,
    play_busy/1, play_congestion/1, play_dial/1, set_auto_hangup/2,
    indicate_congestion/2, indicate_congestion/1, indicate_busy/2, indicate_busy/1,
    indicate_progress/1, get_data/2, get_data/3, get_data/4, send_image/2, send_text/2,
    database_get/3, database_put/4, database_deltree/3, database_deltree/2, database_del/3,
    set_variable/3, get_variable/2, get_full_variable/2, wait_digit/1, wait_digit/2,
    say_phonetic/2, say_phonetic/3, say_alpha/2, say_alpha/3, say_digits/2, say_digits/3,
    say_number/2, say_number/3, say_date/2, say_date/3, say_time/2, say_time/3,
    say_datetime/3, say_datetime/4, say_datetime/5, get_option/3, get_option/4,
    set_context/2, set_priority/2, set_extension/2, fax_send/2, fax_receive/2, dial/4,
    channel_status/1, channel_status/2, record/4, record/5, record/6,
    enable_music/1, enable_music/2, disable_music/1, disable_music/2, set_callerid/3
]).

new_call(Log) ->
    ReadFun = erlagi_io_normal:get_recv_fun(),
    SendFun = erlagi_io_normal:get_send_fun(),
    CloseFun = erlagi_io_normal:get_close_fun(),
    new_call(Log, ReadFun, SendFun, CloseFun)
.

new_call(Log, ReadFun, SendFun, CloseFun) ->
    Env = erlagi_read_env:read(ReadFun),
    #agicall{environment=Env, close=CloseFun, read=ReadFun, send=SendFun, log=Log}
.

terminate(#agicall{} = Call) ->
    erlagi_io:close(Call)
.

set_callerid(#agicall{} = Call, Name, Number) ->
    Clid = erlagi_misc:concat([ [ 34 ], Name, [ 34, 60 ], Number, [ 62 ] ]),
    erlagi_io:agi_rw(Call, "SET CALLERID", [ Clid ])
.

enable_music(#agicall{} = Call) ->
    set_music(Call, "on")
.

enable_music(#agicall{} = Call, Class) ->
    set_music(Call, "on", Class)
.

disable_music(#agicall{} = Call) ->
    set_music(Call, "off")
.

disable_music(#agicall{} = Call, Class) ->
    set_music(Call, "off", Class)
.

set_music(#agicall{} = Call, Flag, Class) ->
    erlagi_io:agi_rw(Call, "SET MUSIC ", [ Flag, Class ])
.

set_music(#agicall{} = Call, Flag) ->
    erlagi_io:agi_rw(Call, "SET MUSIC ", [ Flag ])
.

record(#agicall{} = Call, Args) ->
    erlagi_result:parse_ascii_digit_result(erlagi_io:agi_rw(Call, "RECORD FILE", Args ))
.

record(#agicall{} = Call, Filename, Format, EscapeDigits, MaxMilliseconds, MaxSilenceSeconds)
    ->
    record(Call, [ 
        Filename, Format, EscapeDigits, MaxMilliseconds,
        erlagi_misc:concat([ "s=", MaxSilenceSeconds ])
    ])
.

record(#agicall{} = Call, Filename, Format, EscapeDigits, MaxMilliseconds) ->
    record(Call, [ Filename, Format, EscapeDigits, MaxMilliseconds ])
.

record(#agicall{} = Call, Filename, Format, EscapeDigits) ->
    record(Call, Filename, Format, EscapeDigits, "-1")
.

channel_status(#agicall{} = Call) ->
    erlagi_io:agi_rw(Call, "CHANNEL STATUS")
.

channel_status(#agicall{} = Call, Channel) ->
    erlagi_io:agi_rw(Call, "CHANNEL STATUS", [ Channel ])
.

dial(#agicall{} = Call, Channel, Timeout, Options) ->
    exec(Call, "DIAL", [ Channel, Timeout, Options ])
.

fax_send(#agicall{} = Call, TiffFile) ->
    erlagi_io:agi_rw(Call, "SENDFAX", [ TiffFile, "a" ])
.

fax_receive(#agicall{} = Call, TiffFile) ->
    erlagi_io:agi_rw(Call, "RECEIVEFAX", [ TiffFile ])
.

set_context(#agicall{} = Call, Context) ->
    erlagi_io:agi_rw(Call, "SET CONTEXT", [ Context ])
.

set_extension(#agicall{} = Call, Extension) ->
    erlagi_io:agi_rw(Call, "SET EXTENSION", [ Extension ])
.

set_priority(#agicall{} = Call, Priority) ->
    erlagi_io:agi_rw(Call, "SET PRIORITY", [ Priority ])
.

get_option(#agicall{} = Call, Filename, EscapeDigits, Timeout) ->
    erlagi_result:parse_ascii_digit_result(erlagi_io:agi_rw(
        Call, "GET OPTION", [ Filename, EscapeDigits, Timeout ]
    ))
.

get_option(#agicall{} = Call, Filename, EscapeDigits) ->
    get_option(Call, Filename, EscapeDigits, "0")
.

say_date(#agicall{} = Call, Timestamp) ->
    say_date(Call, Timestamp, "")
.

say_date(#agicall{} = Call, Timestamp, EscapeDigits) ->
    erlagi_result:parse_ascii_digit_result(erlagi_io:agi_rw(
        Call, "SAY DATE", [Timestamp, EscapeDigits]
    ))
.

say_time(#agicall{} = Call, Timestamp) ->
    say_time(Call, Timestamp, "")
.

say_time(#agicall{} = Call, Timestamp, EscapeDigits) ->
    erlagi_result:parse_ascii_digit_result(erlagi_io:agi_rw(
        Call, "SAY TIME", [Timestamp, EscapeDigits]
    ))
.

say_datetime(#agicall{} = Call, Timestamp, EscapeDigits) ->
    erlagi_result:parse_ascii_digit_result(erlagi_io:agi_rw(
        Call, "SAY DATETIME", [Timestamp, EscapeDigits]
    ))
.
say_datetime(#agicall{} = Call, Timestamp, EscapeDigits, Format) ->
    erlagi_result:parse_ascii_digit_result(erlagi_io:agi_rw(
        Call, "SAY DATETIME", [Timestamp, EscapeDigits, Format]
    ))
.
say_datetime(#agicall{} = Call, Timestamp, EscapeDigits, Format, Tz) ->
    erlagi_result:parse_ascii_digit_result(erlagi_io:agi_rw(
        Call, "SAY DATETIME", [Timestamp, EscapeDigits, Format, Tz]
    ))
.

say_phonetic(#agicall{} = Call, Text) ->
    say_phonetic(Call, Text, "")
.

say_phonetic(#agicall{} = Call, Text, EscapeDigits) ->
    erlagi_result:parse_ascii_digit_result(erlagi_io:agi_rw(
        Call, "SAY PHONETIC", [Text, EscapeDigits]
    ))
.

say_alpha(#agicall{} = Call, Text) ->
    say_alpha(Call, Text, "")
.

say_alpha(#agicall{} = Call, Text, EscapeDigits) ->
    erlagi_result:parse_ascii_digit_result(erlagi_io:agi_rw(
        Call, "SAY ALPHA", [Text, EscapeDigits]
    ))
.

say_number(#agicall{} = Call, Text) ->
    say_number(Call, Text, "")
.

say_number(#agicall{} = Call, Text, EscapeDigits) ->
    erlagi_result:parse_ascii_digit_result(erlagi_io:agi_rw(
        Call, "SAY NUMBER", [Text, EscapeDigits]
    ))
.

say_digits(#agicall{} = Call, Text) ->
    say_digits(Call, Text, "")
.

say_digits(#agicall{} = Call, Text, EscapeDigits) ->
    erlagi_result:parse_ascii_digit_result(erlagi_io:agi_rw(
        Call, "SAY DIGITS", [Text, EscapeDigits]
    ))
.

wait_digit(#agicall{} = Call) ->
    erlagi_result:parse_ascii_digit_result(erlagi_io:agi_rw(Call, "WAIT FOR DIGIT", [ "-1" ]))
.

wait_digit(#agicall{} = Call, Milliseconds) ->
    erlagi_result:parse_ascii_digit_result(erlagi_io:agi_rw(
        Call, "WAIT FOR DIGIT", [Milliseconds]
    ))
.

get_variable(#agicall{} = Call, Name) ->
    erlagi_result:get_variable(erlagi_io:agi_rw(
        Call, "GET VARIABLE", [Name]
    ))
.

get_full_variable(#agicall{} = Call, Name) ->
    erlagi_result:get_variable(erlagi_io:agi_rw(
        Call, "GET FULL VARIABLE", [ erlagi_misc:concat([ "${", Name, "}" ]) ]
    ))
.

set_variable(#agicall{} = Call, Name, Value) ->
    erlagi_io:agi_rw(Call, "SET VARIABLE", [Name, Value])
.

verbose(#agicall{} = Call, Message) ->
    erlagi_io:agi_rw(Call, "VERBOSE", [Message])
.

log(#agicall{} = Call, Priority, Message) ->
    exec(Call, "LOG", [Priority, Message])
.

log_notice(#agicall{} = Call, Message) ->
    log(Call, "NOTICE", Message)
.

log_error(#agicall{} = Call, Message) ->
    log(Call, "ERROR", Message)
.

log_dtmf(#agicall{} = Call, Message) ->
    log(Call, "DTMF", Message)
.

log_verbose(#agicall{} = Call, Message) ->
    log(Call, "VERBOSE", Message)
.

log_debug(#agicall{} = Call, Message) ->
    log(Call, "DEBUG", Message)
.

log_warn(#agicall{} = Call, Message) ->
    log(Call, "WARNING", Message)
.

database_deltree(#agicall{} = Call, Family) ->
    database_deltree(Call, Family, "")
.

database_deltree(#agicall{} = Call, Family, Key) ->
    erlagi_io:agi_rw(Call, "DATABASE DELTREE", [Family, Key])
.

database_del(#agicall{} = Call, Family, Key) ->
    database_deltree(Call, Family, Key)
.

database_get(#agicall{} = Call, Family, Key) ->
    erlagi_io:agi_rw(Call, "DATABASE GET", [Family, Key])
.

database_put(#agicall{} = Call, Family, Key, Value) ->
    erlagi_io:agi_rw(Call, "DATABASE PUT", [Family, Key, Value])
.

send_image(#agicall{} = Call, Filename) ->
    erlagi_io:agi_rw(Call, "SEND IMAGE", [Filename])
.

send_text(#agicall{} = Call, Text) ->
    erlagi_io:agi_rw(Call, "SEND TEXT", [Text])
.

hangup(#agicall{} = Call) ->
    erlagi_io:agi_rw(Call, "HANGUP")
.

answer(#agicall{} = Call) ->
    erlagi_io:agi_rw(Call, "ANSWER")
.

stream_file(#agicall{} = Call, Filename) ->
    stream_file(Call, Filename, "")
.

stream_file(#agicall{} = Call, Filename, EscapeDigits) ->
    erlagi_result:parse_ascii_digit_result(erlagi_io:agi_rw(
        Call, "STREAM FILE", [Filename, EscapeDigits]
    ))
.

get_data(#agicall{} = Call, Filename) ->
    get_data(Call, Filename, "", "")
.

get_data(#agicall{} = Call, Filename, Milliseconds) ->
    get_data(Call, Filename, Milliseconds, "")
.

get_data(#agicall{} = Call, Filename, Milliseconds, MaxDigits) ->
    erlagi_result:parse_digit_result(erlagi_io:agi_rw(
        Call, "GET DATA", [Filename, Milliseconds, MaxDigits]
    ))
.

set_auto_hangup(#agicall{} = Call, Seconds) ->
    erlagi_io:agi_rw(Call, "SET AUTOHANGUP", [Seconds]) 
.

indicate_congestion(#agicall{} = Call, Timeout) ->
    exec(Call, "CONGESTION", [Timeout])
.

indicate_progress(#agicall{} = Call) ->
    exec(Call, "PROGRESS")
.

indicate_congestion(#agicall{} = Call) ->
    exec(Call, "CONGESTION")
.

indicate_busy(#agicall{} = Call, Timeout) ->
    exec(Call, "BUSY", [Timeout])
.

indicate_busy(#agicall{} = Call) ->
    exec(Call, "BUSY")
.

play_dial(#agicall{} = Call) ->
    play_tone(Call, "DIAL")
.

play_congestion(#agicall{} = Call) ->
    play_tone(Call, "CONGESTION")
.

play_busy(#agicall{} = Call) ->
    play_tone(Call, "BUSY")
.

play_tone(#agicall{} = Call, Tone) ->
    exec(Call, "PlayTones", [Tone])
.

play_custom_tones(#agicall{} = Call, Freqs) when is_list(Freqs), length(Freqs) > 0 ->
    exec(Call, "PlayTones", Freqs)
.

stop_play_tones(#agicall{} = Call) ->
    exec(Call, "StopPlayTones")
.

form_exec_arguments([]) ->
    ""
;

form_exec_arguments(Arguments) when is_list(Arguments), length(Arguments) > 0 ->
    string:join(Arguments, [44])
.

exec(#agicall{} = Call, Application, Arguments) when is_list(Arguments) ->
    ExecArguments = form_exec_arguments(Arguments),
    ExecCmd = erlagi_misc:concat([ "EXEC ", [34], Application, [34]]),
    erlagi_io:agi_rw(Call, ExecCmd, [ ExecArguments ])
.

exec(#agicall{} = Call, Application) ->
    exec(Call, Application, [])
.


