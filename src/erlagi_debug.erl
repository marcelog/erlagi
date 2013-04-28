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

-export([print_result/1, print_agicall/1]).

print_result(#agiresult{} = Result) ->
    lager:debug(
        "----- AgiResult -----~n"
        ++ "Command: ~s~n"
        ++ "Raw: ~s~n"
        ++ "Result: ~s~n"
        ++ "Digits: ~s~n"
        ++ "Timeout: ~s~n"
        ++ "Offset: ~s~n"
        ++ "Data: ~s~n"
        ++ "---------------------~n", [
        erlagi_result:get_cmd(Result),
        erlagi_result:get_raw(Result),
        erlagi_result:get_result(Result),
        erlagi_result:get_digits(Result),
        erlagi_result:is_timeout(Result),
        erlagi_result:get_offset(Result),
        erlagi_result:get_data(Result)
    ]).

print_variable({Key, Value}) ->
    lager:debug("Variable [ ~s = ~s ]~n", [Key, Value]).

print_agicall(#agicall{environment = Env}) ->
    lager:debug("----- AgiCall -----~n"),
    [ print_variable(V) || V <- Env],
    lager:debug("-------------------~n").


