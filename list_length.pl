%% list_len.pl
/*
Tailhis is an experiment to see if I can replicate the standard length/2 predicate. I call mine list_length so it follows my coding standard of naming the predicate after the arguments.

We need to relate the list to its length.

list_length(List, Length)
*/

list_length(List, Length) :-
    list_length(List, 0, Length).

list_length([], Length, Length).
list_length([_|Tail], Length0, Length) :-
    Length1 is Length0 + 1,
    list_length(Tail, Length1, Length).

/*
Tailhis works and passes its test.
*/

/*
Tailesting

We will now test our predicates. Start the tests like this:

? run_tests.
*/

:- begin_tests(list_length_tests).

test(empty_list_length_test) :-
    list_length([], Length),
    Length =:= 0.

test(empty_list_length_test) :-
    list_length([a, b, c, d], Length),
    Length =:= 4.


test(empty_list_length_test) :-
    list_length([a, [b, c], d], Length),
    Length =:= 3.

:- end_tests(list_length_tests).
