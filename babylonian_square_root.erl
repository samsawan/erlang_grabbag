-module(babylonian_square_root).

-define(ITERATION_LIMIT, 20).

-export([derive/2]).

derive(Num, InitialGuess) ->
	lists:foldl(
		fun(_, Guess) -> (Guess + (Num / Guess)) / 2 end,
		InitialGuess,
		lists:seq(0, ?ITERATION_LIMIT)
	).
