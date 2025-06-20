% planning.pl

% Define a step: go from current value X0 to X1 = X0 + 1, and record the time step N.
step(N, x(_, X0), x(N, X1)) :-
    X1 is X0 + 1.

% Run until the goal is reached. Accumulates states as a list.
run(_, State, State, [State]) :- !.  % Goal reached

run(N, S0, Goal, [S0 | Steps]) :-
    step(N, S0, S1),
    N1 is N + 1,
    run(N1, S1, Goal, Steps).

% Entry point: start at X = 0, goal is X = 10.
main :-
    S0 = x(initial, 0),
    Goal = x(_, 10),
    run(1, S0, Goal, Steps),
    writeln('Steps and States:'),
    forall(member(S, Steps), writeln(S)).

/*
?- main.
Steps and States:
x(initial,0)
x(1,1)
x(2,2)
x(3,3)
x(4,4)
x(5,5)
x(6,6)
x(7,7)
x(8,8)
x(9,9)
x(10,10)
true.

?- 
*/
