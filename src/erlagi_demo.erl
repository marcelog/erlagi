-module(erlagi_demo).

-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-include("erlagi_types.hrl").

-export( [ new_call/1 ]).

run_command(Module, Function, Arguments) ->
    [Call | _] = Arguments,
    Log = Call#agicall.log,
    Result = erlang:apply(Module, Function, Arguments),
    case is_record(Result, agiresult) of
        true -> erlagi_debug:print_result(Log, erlang:apply(Module, Function, Arguments));
        false -> erlagi_log:log(Log, "~s~n", [Result])
    end.

new_call(Call) when is_record(Call, agicall) ->
    run_command(erlagi_debug,print_agicall, [Call]),
    run_command(erlagi, answer, [Call]),
    run_command(erlagi, enable_music, [Call]),
    run_command(erlagi, disable_music, [Call]),
    run_command(erlagi, set_callerid, [Call, "Name", "123"]),
    run_command(erlagi, say_digits, [Call, "123", "#"]),
    run_command(erlagi, say_number, [Call, "123", "#"]),
    run_command(erlagi, wait_digit, [Call ]),
    run_command(erlagi, set_variable, [Call, "Some", "Value"]),
    run_command(erlagi, log_notice, [Call, erlagi:get_variable(Call, "Some")]),
    run_command(erlagi, log_notice, [Call, erlagi:get_full_variable(Call, "Some")]),
    run_command(erlagi, set_variable, [Call, "Some", "\"Value\""]),
    run_command(erlagi, log_notice, [Call, erlagi:get_variable(Call, "Some")]),
    run_command(erlagi, log_notice, [Call, erlagi:get_full_variable(Call, "Some")]),
    run_command(erlagi, log_notice, [Call, "Hello Joe"]),
    run_command(erlagi, log_debug, [Call, "Hello \"Mike\""]),
    run_command(erlagi, log_warn, [Call, "Hi"]),
    run_command(erlagi, log_error, [Call, "Hi"]),
    run_command(erlagi, log_verbose, [Call, "Hi"]),
    run_command(erlagi, log_dtmf, [Call, "Hi"]),
    run_command(erlagi, database_deltree, [Call, "Blah"]),
    run_command(erlagi, database_deltree, [Call, "Blah", "bleh"]),
    %run_command(erlagi, set_auto_hangup, [Call, "3"]),
    run_command(erlagi, stream_file, [Call, "welcome", "#"]),
    run_command(erlagi, stop_play_tones, [Call]),
    run_command(erlagi, play_custom_tones, [Call, [
        "!350+440/100","!0/100","!350+440/100","!0/100","!350+440/100","!0/100","350+440"
    ]]),
    timer:sleep(2000),
    run_command(erlagi, stop_play_tones, [Call]),
    run_command(erlagi, play_busy, [Call]),
    run_command(erlagi, indicate_busy, [Call, "2"]),
    timer:sleep(2000),
    run_command(erlagi, stop_play_tones, [Call]),
    run_command(erlagi, play_congestion, [Call]),
    run_command(erlagi, indicate_congestion, [Call, "2"]),
    timer:sleep(2000),
    run_command(erlagi, stop_play_tones, [Call]),
    run_command(erlagi, play_dial, [Call]),
    timer:sleep(2000),
    run_command(erlagi, stop_play_tones, [Call]),
    run_command(erlagi, record, [Call, "file", "wav", "#", "3000"]),
    run_command(erlagi, dial, [Call, "SIP/54115555555", "3", "hH"]),
    run_command(erlagi, hangup, [Call]),
    run_command(erlagi, terminate, [Call]).

