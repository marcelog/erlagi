-module(erlagi_misc).

-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-export([concat/2,concat/1] ).

% just to not use ++ to concat strings
concat([], Tail) ->
    Tail;

concat([H | T], Tail) ->
    [H | concat(T, Tail)].

concat([]) ->
    [];

concat([H | T]) ->
    concat(H, concat(T)).

