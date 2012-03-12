-module(erlagi_debug).

-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-include("erlagi_types.hrl").

-import(io).
-import(lists).
-import(erlagi_log).
-import(erlagi_result).

-export( [ print_result/2, print_agicall/1 ] ).

print_result(Log, Result) when is_record(Result, agiresult) ->
    erlagi_log:log(Log, "----- AgiResult -----~n"),
    erlagi_log:log(Log, "Command: ~s~n", [ erlagi_result:get_cmd(Result) ]),
    erlagi_log:log(Log, "Raw: ~s~n", [ erlagi_result:get_raw(Result) ]),
    erlagi_log:log(Log, "Result: ~s~n", [ erlagi_result:get_result(Result) ]),
    erlagi_log:log(Log, "Digits: ~s~n", [ erlagi_result:get_digits(Result) ]),
    erlagi_log:log(Log, "Timeout: ~s~n", [ erlagi_result:is_timeout(Result) ]),
    erlagi_log:log(Log, "Offset: ~s~n", [ erlagi_result:get_offset(Result) ]),
    erlagi_log:log(Log, "Data: ~s~n", [ erlagi_result:get_data(Result) ]),
    erlagi_log:log(Log, "---------------------~n")
.

print_agicall(Call) when is_record(Call, agicall) ->
    Log = Call#agicall.log,
    erlagi_log:log(Log, "----- AgiCall -----~n"),
    lists:map(
        fun({Key, Value}) ->
            erlagi_log:log(Log, "Variable [ ~s = ~s ]~n", [ Key, Value ])
        end,
        Call#agicall.environment
    ),
    erlagi_log:log(Log, "-------------------~n")
.


