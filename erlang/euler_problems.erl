-module(euler_problems).
-author("Daniel Underwood").
-export([solve_problem/2]).

%% -------- Entry point --------

%% Solves the given Euler Problem with supplied Args
solve_problem(1, {Max, Factors}) ->
	sum_multiples(Max, Factors);
solve_problem(_, _Args) ->
	io:format("Unable to solve problem. Check to make sure problem"
		  " solution is implemented.~n").

%%  -------- Problem Solutions --------

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

%% Second Euler Problem. Sums the even numbers in the Fibonacci sequence below
%% Max. Run with N=33 for solution to given for actual Euler Problem.
fibonacci_even_sum(Max) when is_integer(Max), (Max > 0) ->
	Fib_Sequence = [fibonacci(X) || X <- lists:seq(1, Max)],
	Even_Fibs = [X || X <- Fib_Sequence, X rem 2 =:= 0],
	lists:sum(Even_Fibs);
fibonacci_even_sum(_) ->
	io:format("Badly formatted maximum. Make sure it is a number greater "
		  "than 0.~n"),
	error.

%% -------- Utility Functions --------

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
