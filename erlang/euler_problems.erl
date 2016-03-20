-module(euler_problems).
-author("Daniel Underwood").
-export([solve_problem/2]).

%% Solves the given Euler Problem with supplied Args
solve_problem(1, {Max, Factors}) ->
	sum_multiples(Max, Factors);
solve_problem(_, _Args) ->
	io:format("Unable to solve problem. Check to make sure problem"
		  " solution is implemented.~n").

%% First Euler Problem. Checks if integers lower than Max are multiples of
%% numbers in Factors and then sums those multiples
sum_multiples(Max, Factors) when is_number(Max) and
				 (is_integer(Factors) or is_list(Factors)) ->
	Multiples = [X || X <- lists:seq(1, Max-1),
			  list_has_factor(X, Factors)],
	lists:sum(Multiples);
sum_multiples(_, _) ->
	io:format("Badly formatted arguments. Check types.~n"),
	error.

%% Determined whether Num has a factor in list that is passed as second argument
list_has_factor(Num, [Factor|_]) when Num rem Factor =:= 0 ->
	true;
list_has_factor(Num, [Factor|Remaining]) when Num rem Factor =/= 0 ->
	list_has_factor(Num, Remaining);
list_has_factor(_, List) when not is_list(List) ->
	io:format("~p is not a list or number. Second argument must be list or"
		  " number.~n", [List]),
	error;
list_has_factor(_, _) ->
	false.
