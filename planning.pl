% planning.pl
% This appears to be "pure" prolog to me. I think this is a good example of using
% findall which seems to be a currently favored predicate in the "pure" prolog 

% One step: move from X to X+1, record time step.
step(N, x(_, X0), x(N, X1)) :-
    X1 is X0 + 1.

% Generate all steps up to a max goal value.
run_findall(Max, Steps) :-
    findall(
        x(N, X),
        (
            between(0, Max, N),
            X is N
        ),
        Steps
    ).

main :-
    GoalValue = 10,
    run_findall(GoalValue, Steps),
    writeln('Steps and States:'),
    forall(member(S, Steps), writeln(S)).
