%% conc.pl

:- module(conc, [
    conc/3
]).

/*
Experiment with home-grown concatentation predicate. It's only for academic value. Use append/3 instead of this. 
*/

conc([], L, L).
conc([X|L1], L2, [X|L3]) :-
    conc(L1, L2, L3).

:- begin_tests(concatenation_tests).

test(empty_first_list) :-
    conc([], [1, 2, 3], L),
    L == [1, 2, 3].

test(empty_second_list) :-
    conc([a, b, c], [], L),
    L == [a, b, c].

test(both_empty) :-
    conc([], [], L),
    L == [].

test(flat_concatenation) :-
    conc([a, b, c], [1, 2, 3], L),
    L == [a, b, c, 1, 2, 3].

test(nested_elements) :-
    conc([a, [b, c], d], [a, [], b], L),
    L == [a, [b, c], d, a, [], b].

test(reversible_concatenation, [nondet]) :-
    conc(L1, [b], [a, b]),
    L1 == [a].

test(instantiated_result, [nondet]) :-
    conc(L1, L2, [x, y, z]),
    L1 == [x],
    L2 == [y, z].

:- end_tests(concatenation_tests).
