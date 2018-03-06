-module(transpose).

-export([transpose/1]).
-include_lib("eunit/include/eunit.hrl").

transpose(_) -> "".

empty_string_test() ->
	?assertEqual("", transpose("")).
