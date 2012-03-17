-module(erlagi_io_normal).

-import(io).

-export( [ get_close_fun/0, get_send_fun/0, get_recv_fun/0 ] ).

get_close_fun() ->
    fun() -> 
        ok
    end
.

get_send_fun() ->
    fun(Text) ->
        io:format("~s", [Text])
    end
.

get_recv_fun() ->
    fun() ->
        io:get_line(standard_io, "")
    end
.

