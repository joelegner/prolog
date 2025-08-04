% robot.pl
/*
Ivan Bratko section 4.2

Represent the state of the world with a structure (compound term).
state(arg1, arg2, arg3).

arg1: Location of the robot.
arg2: Location of the basket.
arg3: Location of the rubbish.

So the initial state looks like this:
*/
state(door, corner2, floor(middle)).

/*
The final goal is for the rubbish to be in the basket.

We _could_ represent this state like this:
state(corner2, corner2, in_basket).

But why bother with the robot and basket locations? Argument arg3 is enough.

We can now formulate the GOAL of the robot:
*/
state(_, _, in_basket).

/*
What about actions?

A1: pickup, pick up rubbish from floor
A2: drop, drop rubbish into basket. 
A3: push(Pos1, Pos2), push basket from position Pos1 to Pos2
A4: go(Pos1, Pos2), go from Pos1 to Pos2.  

Notice the order of the actions. If we declare the actions in this order, then it represents the order of precedent. This means when Prolog needs to decide between a pair of available actions, it will pick the action that was declared first. 

The general representation of an action includes a starting state, and end state, and an action in between. 

action(State1, Action, State2).

          Action
State1 --------------> State2
*/

/*
Let's look at the action schema for the "pickup" action, A1 above. We can pick up only when the rubbish position is on the floor. Once we pick up, the rubbish is held.
*/
action(
    state(Pos1, Pos2, floor(Pos1)),
    pickup,
    state(Pos1, Pos2, held)
).


/*
Consider the action "drop" labeled A2 above. It can only be performed when the robot is at the trash bin.

Let's look at an "action schema" for the drop action. You can identify an action schema by the presence of variables. The drop action takes no arguments.
*/
action(
    state(Pos, Pos, held),
    drop,
    state(Pos, Pos, in_basket)
).

/*
We need an action schema for the "push" move, A3. The push action takes two arguments, a previous position and a new position.
*/
action(
    state(Pos, Pos, Loc),
    go(Pos, NewPos),
    state(NewPos, NewPos, Loc)
).

/*
Let's look at the action schema for the go action, A4.
*/
action(
    state(Pos1, Pos2, Loc),
    go(Pos1, NewPos1),
    state(NewPos1, Pos2, Loc)
).

/*
Now we want to do the computations using our model. We want to compute a plan of actions that move from a starting state to an ending state. Can the robot, starting in some initial state, clean rubbish into the basket? If yes, what is the sequence of steps to do that? 

This is the essence of planning, in Life as in Prolog. 

We can represent a plan like this:
plan(StartState, GoalState, Plan).

Plan is a sequence of actions. We might consider refactoring:

plan(StartState, GoalState, Actions). 

This predicate is true if there exists a sequence of possible actions that change the start state to the goal state. 

Do we call the code below the "plan schema"?

If the start state is already the goal state, we are done. Our action list is an empty list. This is the first clause in the procedure. 
*/
plan(StartState, StartState, []).

/*
If the first clause does not resolve, one or more actions will be necessary to make the plan true. 
*/
plan(State1, GoalState, [Action1|RestOfPlan]) :-
    action(State1, Action1, State2),
    plan(State2, GoalState, RestOfPlan).

/*
This query will find the plan to hold the rubbish and stop there.

?- State0 = state(door, corner2, floor(middle)), plan(State0, state(_, _, held), Plan). 
State0 = state(door,corner2,floor(middle)),
Plan = [go(door,middle),pickup] ;

The key here is that Goal = state(_, _, held). We do not care about anything but the rubbish being held. Yet still the simple plan finding procedure works. 
*/

/*
To see how flexible this planner is consider this query.

?- S0 = state(point(0, 200), point(600, 400), floor(point(300, 200))), plan(S0, state(_, _, in_basket), Plan). % Query 1

We have replaced door, corner2, and so on with coordinates. To do this we just inserted the points into the query. There is no other code than what you see here. And by jove it works. Here is the output of Query 1.

S0 = state(point(0,200),point(600,400),floor(point(300,200))),
Plan = [go(point(0,200),point(300,200)),pickup,go(point(300,200),point(600,400)),drop]
*/