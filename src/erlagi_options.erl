%%%
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


