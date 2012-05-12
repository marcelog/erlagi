%%% Useful to debug the IVR.
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
-module(erlagi_debug).

-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-include("erlagi_types.hrl").

-export([print_result/2, print_agicall/1]).

print_result(Log, #agiresult{} = Result) ->
    erlagi_log:log(Log, "----- AgiResult -----~n"),
    erlagi_log:log(Log, "Command: ~s~n", [ erlagi_result:get_cmd(Result) ]),
    erlagi_log:log(Log, "Raw: ~s~n", [ erlagi_result:get_raw(Result) ]),
    erlagi_log:log(Log, "Result: ~s~n", [ erlagi_result:get_result(Result) ]),
    erlagi_log:log(Log, "Digits: ~s~n", [ erlagi_result:get_digits(Result) ]),
    erlagi_log:log(Log, "Timeout: ~s~n", [ erlagi_result:is_timeout(Result) ]),
    erlagi_log:log(Log, "Offset: ~s~n", [ erlagi_result:get_offset(Result) ]),
    erlagi_log:log(Log, "Data: ~s~n", [ erlagi_result:get_data(Result) ]),
    erlagi_log:log(Log, "---------------------~n").

print_variable(Log, {Key, Value}) ->
    erlagi_log:log(Log, "Variable [ ~s = ~s ]~n", [ Key, Value ]).

print_agicall(#agicall{environment = Env, log = Log}) ->
    erlagi_log:log(Log, "----- AgiCall -----~n"),
    [ print_variable(Log, V) || V <- Env],
    erlagi_log:log(Log, "-------------------~n").


