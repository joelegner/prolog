% planning.pl

/*
What I'm going for here is the simplest planning scenario I could think of. It works. So the next thing would be to expand the system. Every complex system that works evolved from a simpler system that worked. 

As an added benefit, this appears to be "pure" prolog. I think this is a good example of using `findall` which seems to be a currently favored predicate among the "pure" prolog proponents.

The situation is this:

There is an integer number line. In the starting state, our marker is at the origin, zero. Each turn or "step" we can move the marker one to the right on the number line. 

There are only values from the origin to the goal. After that there are no solutions. This is the better way to think about it. Do not think about it like a loop or even an if-then statement. Just say that the relationship only holds between the origin and the goal, inclusive. 
*/

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

/*
Example run:

?- main.
Steps and States:
x(0,0)
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
*/