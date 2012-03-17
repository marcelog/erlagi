-module(erlagi_options).

-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-import(erlagi_defaults).

-export( [ get_option/2 ] ).

get_option(Key, Options) ->
    Value = [X || { CandidateKey, X } <- Options, Key =:= CandidateKey],
    case Value of
        [] -> get_default_option(Key);
        [H | _] -> H
    end
.

get_default_option(Key) ->
    get_option(Key, erlagi_defaults:get_default_options())
.


