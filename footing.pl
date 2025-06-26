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

:- use_module(library(clpfd)).

input_output(Input, Output) :-
    Output #= Input + 1.

:- begin_tests(input_output_tests).

test(input_to_output) :-
    input_output(24, Output),
    Output #= 25.

test(output_to_input) :-
    input_output(Input, 30),
    Input #= 29.

test(both_known_true) :-
    input_output(5, 6).

test(both_known_false, [fail]) :-
    input_output(5, 7).

:- end_tests(input_output_tests).