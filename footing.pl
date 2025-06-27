%% footing.pl

/*
Started over on June 25, 2025 with my latest state-of-the-art knowledge level. 

Here are some points to make as we proceed this time.

1. Consider using a DCG for the input definition. There is phrase_to_file predicate.

2. Consider using a DCG for the output definition. I made a phrase_from_file predicate.

3. Predicate names: term1_term1(Term1, Term2). In other words, the predicate name should be the two objects the predicate relates. This gives us the right order of arguments, too. And it leads to purer code. It gets us to think _declaratively_. 

4. Predicate should ideally work in two directions.

5. We should test every predicate, including in novel ways.

With these things in mind, what is the problem we are solving? 

If we are using Prolog, we can reformulate the question:

What relation are we trying establish?

We are trying to establish the relation of input to output. Let's start there.
*/ 

% footing.pl

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

% This is the core predicate relating definition and solution.
problem_definition_solution(Definition, Solution) :-
    phrase(solution(Definition), Solution).

% DCG: maps each 'a' in Definition to a 'b' in Solution
solution([])     --> [].
solution([a|As]) --> [b], solution(As).

/*
TODO: Define better DCG for footing design. Some thoughts.
*/

:- begin_tests(problem_definition_solution).

test(def_to_soln) :-
    problem_definition_solution([a,a,a], Solution),
    Solution == [b,b,b].

test(soln_to_def) :-
    once(problem_definition_solution(Definition, [b,b])),
    Definition == [a,a].

test(both_known_true) :-
    problem_definition_solution([a,a], [b,b]).

test(both_known_false, [fail]) :-
    problem_definition_solution([a,a], [b,b,b]).

test(non_a_input, [fail]) :-
    problem_definition_solution([x], _).

test(non_b_output, [fail]) :-
    problem_definition_solution(_, [x]).

:- end_tests(problem_definition_solution).

% My default should be to append new clauses on existing programs, unless it's time for refactoring.

/*
This algorithm from Bratko inspires me:

procedure execute(Program, GoalList, Success);

What I want my footing design to be is this:

Loads:
D = Dead
L = Live
S = Snow
*/
run :-
    soil_loads_area(2000, [40, 100, 30], Area),
    area_square_width(Area, Width),
    format('Area = ~w sq ft, Width = ~w ft~n', [Area, Width]).

soil_loads_area(Soil, Loads, Area) :-
    loads_combined(Loads, Combinations),
    soil_combos_area(Soil, Combinations, Area).

loads_combined(Loads, Combinations) :-
    Loads = [D, L, _], 
    Combo1 #= D + L,
    Loads = [D, _, S], 
    Combo2 #= D + S,
    Combinations = [Combo1, Combo2].

soil_combos_area(Soil, Combinations, Area) :-
    max_list(Combinations, P),
    P1 #= P * 1000,
    Area #= P1 div Soil.

/*
This is a good start. My way of thinking worked well. Here's how it went. Take loads_combined for the first example. It relates the list of Loads to a list of Load Combination values. The first thing I did was write the predicate:

loads_combined(Loads, Combinations) :-

Now I know I need to get from loads to combinations. The first thing to do is to extract dead and live loads. So I added the lines uniting loads with lists including variables.

loads_combined(Loads, Combinations) :-
    Loads = [D, L, _], 
    Loads = [D, _, S], 

I need to calculate the load combinations. Let's add that code using the #= operator from CLP(FD).

loads_combined(Loads, Combinations) :-
    Loads = [D, L, _], 
    Combo1 #= D + L,
    Loads = [D, _, S], 
    Combo2 #= D + S,

Finally, we just need to wrap those combined loads into a list:

loads_combined(Loads, Combinations) :-
    Loads = [D, L, _], 
    Combo1 #= D + L,
    Loads = [D, _, S], 
    Combo2 #= D + S,
    Combinations = [Combo1, Combo2].

?- make, run. % Gave me the right answer.

This is how my mind worked writing this program. I think it's the right way to do it.
*/

area_square_width(Area, SquareWidth) :- 
    SquareWidth in 1..100,
    SquareWidth*SquareWidth #> Area,
    indomain(SquareWidth).

/*
This gives you the first solution which is what we want for a footing design.

area_square_width(Area, SquareWidth) :- 
    SquareWidth in 1..100,
    SquareWidth*SquareWidth #> Area,
    indomain(SquareWidth).

?- run.
Area = 70 sq ft, Width = 9 ft
?- make, run.

This gives you many solutions.

area_square_width(Area, SquareWidth) :- 
    SquareWidth in 1..100,
    SquareWidth*SquareWidth #> Area,
    label([SquareWidth]).

?- run.
Area = 70 sq ft, Width = 9 ft
Area = 70 sq ft, Width = 10 ft
Area = 70 sq ft, Width = 11 ft
Area = 70 sq ft, Width = 12 ft
...
*/