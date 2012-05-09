-module(erlagi_io_tcp).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-export([get_close_fun/1, get_send_fun/1, get_recv_fun/1]).

get_close_fun(Socket) ->
    fun() ->
        gen_tcp:close(Socket)
    end.

get_send_fun(Socket) ->
    fun(Text) ->
        ok = gen_tcp:send(Socket, Text)
    end.

get_recv_fun(Socket) ->
    fun() ->
        {ok, Text} = gen_tcp:recv(Socket, 0),
        Text
    end.

