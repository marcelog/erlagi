%%% Used to access and manipulate the result after executing an AGI command.
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
-module(erlagi_result).

-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-include("erlagi_types.hrl").

-export( [
    parse_result/2, parse_ascii_digit_result/1, parse_digit_result/1,
    get_digits/1, has_input/1, is_timeout/1, has_data/1, get_data/1,
    get_raw/1, get_cmd/1, get_variable/1, get_result/1, get_offset/1
] ).

get_data_from_result(X) ->
    case X of
        [] -> "";
        _ when is_list(X) -> string:join(X, " ");
        _ -> X
    end.

get_offset_from_data(Text) ->
    Index = string:str(Text, "endpos="),
    case Index of
        0 -> false;
        _ -> string:right(Text, string:len(Text) - Index - 6)
    end.

get_timeout_from_data(ResultData) ->
    string:str(ResultData, "timeout") =/= 0 orelse
        string:str(ResultData, "dtmf") =:= 0.

get_result_from_raw(Text) ->
    Index = string:str(Text, [61]), % "
    string:right(Text, string:len(Text) - Index).

split_result_line(Text) ->
    string:tokens(Text, [32]).

check_result(ResultExploded) when is_list(ResultExploded) ->
    case ResultExploded of
        [ "HANGUP" | _ ] -> ResultExploded;
        [ "200" | _ ] -> ResultExploded;
        _ -> erlang:error( { invalid_result, string:join(ResultExploded, " ") })
    end.

parse_result(Cmd, Text) ->
    ResultExploded = split_result_line(Text),
    [_ | DataExploded] = check_result(ResultExploded),
    [ResultString | T] = DataExploded,
    ResultData = get_data_from_result(T),
    Result = get_result_from_raw(ResultString),
    Offset = get_offset_from_data(ResultData),
    Timeout = get_timeout_from_data(ResultData),
    #agiresult{
        result=Result,timeout=Timeout, offset=Offset, data=ResultData, cmd=Cmd, raw=Text
    }.

parse_digit_result(#agiresult{} = Result) ->
    #agiresult{
        result = get_result(Result),
        timeout = is_timeout(Result),
        digits = get_result(Result),
        offset = get_offset(Result),
        data = get_data(Result),
        cmd = get_cmd(Result),
        raw = get_raw(Result)
    }.

parse_ascii_digit_result(#agiresult{} = Result) ->
    { Char, _ } = string:to_integer(get_result(Result)),
    #agiresult{
        result = get_result(Result),
        timeout = get_result(Result) =:= "0",
        digits = get_result(Result),
        offset = get_offset(Result),
        data = [ Char ],
        cmd = get_cmd(Result),
        raw = get_raw(Result)
    }.

get_result(#agiresult{result = Result}) ->
    Result.

get_cmd(#agiresult{cmd = Cmd}) ->
    Cmd.

get_raw(#agiresult{raw = Raw}) ->
    Raw.

get_offset(#agiresult{offset = Offset}) ->
    Offset.

get_digits(#agiresult{digits = Digits}) ->
    Digits.

has_input(#agiresult{digits = Digits}) ->
    Digits =/= false.

get_data(#agiresult{data = Data}) ->
    Data.

has_data(#agiresult{data = Data}) ->
    Data =/= false.

is_timeout(#agiresult{timeout = Timeout}) ->
    Timeout.

has_valid_variable(#agiresult{result = Result}) ->
    get_result_from_raw(Result) =:= "1".

get_variable(#agiresult{} = Result) ->
    case has_valid_variable(Result) of
        true ->
            Text = get_data(Result),
            string:substr(Text, 2, string:len(Text) - 2);
        false -> false
    end.

