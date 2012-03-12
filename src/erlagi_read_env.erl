-module(erlagi_read_env).

-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-import(erlagio_io).
-import(erlang_read_env).

-export( [ read/1 ] ).

split_lines(Text) ->
    string:tokens(Text, [ 10 ])
.

%% Returns a tuple, { Key, Value }
parse_variable(Text) ->
    Index = string:str(Text, ":"),
    Key = string:left(Text, Index),
    Value = string:right(Text, string:len(Text) - Index),
    { Key, Value }
.

parse_variables(Text) ->
    lists:map(fun(X) -> parse_variable(X) end, split_lines(Text))
.

find_end_of_variables(Text) ->
    string:str(Text, [ 10, 10 ])
.

has_end_of_variables(Text) ->
    Index = find_end_of_variables(Text),
    case Index of
        0 -> false;
        _ -> true
    end
.

remove_end_of_variables(Text) ->
    string:left(Text, string:len(Text) - 2)
.

read_loop(ReadFun, Buffer) ->
    Text = string:concat(Buffer, ReadFun()),
    case has_end_of_variables(Text) of
        false -> read_loop(ReadFun, Text);
        true -> remove_end_of_variables(Text)
    end
.

read(ReadFun) ->
    parse_variables(read_loop(ReadFun, ""))
.


