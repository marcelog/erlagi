-module(erlagi_result).

-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-include("erlagi_types.hrl").

-import(string).

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
    end
.

get_offset_from_data(Text) ->
    Index = string:str(Text, "endpos="),
    case Index of
        0 -> false;
        _ -> string:right(Text, string:len(Text) - Index - 6)
    end
.

get_timeout_from_data(ResultData) ->
    string:str(ResultData, "timeout") =/= 0
    orelse string:str(ResultData, "dtmf") =:= 0
.

get_result_from_raw(Text) ->
    Index = string:str(Text, [ 61 ]), % "
    string:right(Text, string:len(Text) - Index)
.

split_result_line(Text) ->
    string:tokens(Text, [ 32 ])
.

check_result(ResultExploded) when is_list(ResultExploded) ->
    case ResultExploded of
        [ "HANGUP" | _ ] -> erlang:error(call_terminated);
        [ "200" | _ ] -> ResultExploded;
        _ -> erlang:error( { invalid_result, string:join(ResultExploded, " ") })
    end
.

parse_result(Cmd, Text) ->
    ResultExploded = split_result_line(Text),
    [ _ | DataExploded ] = check_result(ResultExploded),
    [ ResultString | T ] = DataExploded,
    ResultData = get_data_from_result(T),
    Result = get_result_from_raw(ResultString),
    Offset = get_offset_from_data(ResultData),
    Timeout = get_timeout_from_data(ResultData),
    #agiresult{
        result=Result,timeout=Timeout, offset=Offset, data=ResultData, cmd=Cmd, raw=Text
    }
.

parse_digit_result(Result) when is_record(Result, agiresult) ->
    #agiresult{
        result = get_result(Result),
        timeout = is_timeout(Result),
        digits = get_result(Result),
        offset = get_offset(Result),
        data = get_data(Result),
        cmd = get_cmd(Result),
        raw = get_raw(Result)
    }
.

parse_ascii_digit_result(Result) when is_record(Result, agiresult) ->
    { Char, _ } = string:to_integer(get_result(Result)),
    #agiresult{
        result = get_result(Result),
        timeout = get_result(Result) =:= "0",
        digits = get_result(Result),
        offset = get_offset(Result),
        data = [ Char ],
        cmd = get_cmd(Result),
        raw = get_raw(Result)
    }
.

get_result(Result) when is_record(Result, agiresult) ->
    Result#agiresult.result
.

get_cmd(Result) when is_record(Result, agiresult) ->
    Result#agiresult.cmd
.

get_raw(Result) when is_record(Result, agiresult) ->
    Result#agiresult.raw
.

get_offset(Result) when is_record(Result, agiresult) ->
    Result#agiresult.offset
.

get_digits(Result) when is_record(Result, agiresult) ->
    Result#agiresult.digits
.
 
has_input(Result) when is_record(Result, agiresult) ->
    Result#agiresult.digits =/= false
.

get_data(Result) when is_record(Result, agiresult) ->
    Result#agiresult.data
.

has_data(Result) when is_record(Result, agiresult) ->
    Result#agiresult.data =/= false
.
 
is_timeout(Result) when is_record(Result, agiresult) ->
    Result#agiresult.timeout
.

has_valid_variable(Result) when is_record(Result, agiresult) ->
    get_result(Result) =:= "1"
.

get_variable(Result) when is_record(Result, agiresult) ->
    case has_valid_variable(Result) of
        true ->
            Text = get_data(Result),
            string:substr(Text, 2, string:len(Text) - 2);
        false -> false
    end
.

