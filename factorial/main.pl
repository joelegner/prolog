% main.pl
% Main predicate for factorial_exec proof of concept.
main :-
    factorial(5, F),
    format('Factorial of 5 is ~w~n', [F]).
