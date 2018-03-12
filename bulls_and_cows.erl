-module(bulls_and_cows).

-export([play/0]).

play() -> play(generate_secret_number(), 1).

% internal

play(SecretNumber, 7) ->
	io:format("you lose! good day sir!~nthe answer was ~p, you dummy~n", [SecretNumber]);
play(SecretNumber, TurnCount) ->
	io:format("Turn ~p of 7~n", [TurnCount]),
	UserGuess = get_users_guess(),
	EvaluatedGuess = evaluate_guess(UserGuess, SecretNumber),
	case is_winner(EvaluatedGuess) of
		true ->
			io:format("you win! well done~n");
		false ->
			print_cows_and_bulls_info(EvaluatedGuess),
			play(SecretNumber, TurnCount + 1)
	end.

generate_secret_number() -> generate_secret_number([]).

generate_secret_number(NumberList) when length(NumberList) =:= 4 ->
	NumberList;
generate_secret_number(NumberList) ->
	NumberToAdd = rand:uniform(9),
	case lists:member(NumberToAdd, NumberList) of
		true -> generate_secret_number(NumberList);
		false -> generate_secret_number([NumberToAdd | NumberList])
	end.

get_users_guess() ->
	{ok, Input} = io:fread("please enter guess (four distinct numbers between 1 and 9): ", "~d~d~d~d"),
	Input.

print_cows_and_bulls_info([{cows, CowNumber}, {bulls, BullNumber}]) ->
	io:format("You have ~p cows and ~p bulls~n", [CowNumber, BullNumber]).

evaluate_guess(Guess, NumberList) ->
	element(2,
		lists:foldl(
			fun(IndividualGuess, {Index, CowsAndBullsCount}) ->
				case is_bull(IndividualGuess, Index, NumberList) of
					true ->
						{Index + 1, increment_bulls(CowsAndBullsCount)};
					false ->
						case is_cow(IndividualGuess, NumberList) of
							true -> {Index + 1, increment_cows(CowsAndBullsCount)};
							false -> {Index + 1, CowsAndBullsCount}
						end
					end
				end,
			{1, [{cows, 0}, {bulls, 0}]},
			Guess
		)
	).

is_bull(Guess, Pos, SecretList) -> lists:nth(Pos, SecretList) =:= Guess.
is_cow(Guess, NumberList) -> lists:member(Guess, NumberList).

increment_cows([{cows, CowCount}, BullTuple]) -> [{cows, CowCount + 1}, BullTuple].
increment_bulls([CowTuple, {bulls, BullCount}]) -> [CowTuple, {bulls, BullCount + 1}].

is_winner([{cows, 0}, {bulls, 4}]) -> true;
is_winner(_) -> false.

% tests

-include_lib("eunit/include/eunit.hrl").

% todo add test for numbers being between 1 and 9
generate_secret_numbers_test_() ->
	[
		{"the number list has 4 things in it", ?_assertEqual(4, length(generate_secret_number()))},
		{"the number list is unique each time", generate_unique_assertion()}
	]. %++ test_for_randomness().

% todo list comprehension
test_for_randomness() -> lists:map(fun(_) -> {"randomness test", generate_unique_assertion()} end, lists:seq(1, 100)).

generate_unique_assertion() ->
	First = generate_secret_number(),
	Second = generate_secret_number(),
	?_assertNotEqual(First, Second).

evaluate_guess_test_() ->
	[
		{"no cows or bulls", ?_assertEqual([{cows, 0}, {bulls, 0}], evaluate_guess([2,9,8,7], [1,3,5,6]))},
		{"no cows one bull", ?_assertEqual([{cows, 0}, {bulls, 1}], evaluate_guess([1,2,3,4], [9,2,5,6]))},
		{"one cow no bulls", ?_assertEqual([{cows, 1}, {bulls, 0}], evaluate_guess([1,2,3,4], [2,9,5,6]))},
		{"one cows one bull", ?_assertEqual([{cows, 1}, {bulls, 1}], evaluate_guess([1,9,3,7], [1,3,5,6]))},
		{"four bulls", ?_assertEqual([{cows, 0}, {bulls, 4}], evaluate_guess([1,2,3,4], [1,2,3,4]))}
	].

evaluate_winner_test_() ->
	[
		{"not a winner", ?_assertNot(is_winner([{cows, 2}, {bulls, 0}]))},
		{"is a winner", ?_assert(is_winner([{cows, 0}, {bulls, 4}]))}
	].

