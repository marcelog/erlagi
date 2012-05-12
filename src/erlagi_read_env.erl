%%% This modules parses the channel (environment) variables passed by
%%% Asterisk in the AGI handshake.
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
-module(erlagi_read_env).

-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-export([read/1]).

split_lines(Text) ->
    string:tokens(Text, [10]).

%% Returns a tuple, { Key, Value }
parse_variable(Text) ->
    Index = string:str(Text, ":"),
    Key = string:left(Text, Index),
    Value = string:right(Text, string:len(Text) - Index),
    { Key, Value }.

parse_variables(Text) ->
    [parse_variable(X) || X <- split_lines(Text)].

find_end_of_variables(Text) ->
    string:str(Text, [10, 10]).

has_end_of_variables(Text) ->
    Index = find_end_of_variables(Text),
    Index =/= 0.

remove_end_of_variables(Text) ->
    string:left(Text, string:len(Text) - 2).

read_loop(ReadFun, Buffer) ->
    Text = string:concat(Buffer, ReadFun()),
    case has_end_of_variables(Text) of
        false -> read_loop(ReadFun, Text);
        true -> remove_end_of_variables(Text)
    end.

read(ReadFun) ->
    parse_variables(read_loop(ReadFun, "")).


