% factorial.pl
% https://www.swi-prolog.org/pldoc/man?section=cmdlinecomp
%
% factorial(N, F) means F is the factorial of N
factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

main :-
    factorial(5, F),
    format('Factorial of 5 is ~w~n', [F]).

