-module(ackerman_function).

-export([ackerman_function/2]).

ackerman_function(0, N) -> N + 1;
ackerman_function(M, N) when M > 0 andalso N =:= 0 -> ackerman_function(M - 1, 1);
ackerman_function(M, N) when M > 0 andalso N > 0 -> ackerman_function(M - 1, ackerman_function(M, N - 1)).
