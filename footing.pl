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
