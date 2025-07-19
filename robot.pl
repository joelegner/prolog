% robot.pl
% Don't get excited -- this was written by Claude AI on July 18, 2025.

% ============================================================================
% AUTOMATED ROBOT PLANNING SYSTEM
% ============================================================================
% This system demonstrates classical AI planning using pure Prolog.
% 
% ARCHITECTURE FLOW:
% 1. DOMAIN MODEL - Defines the basic entities in our world (rooms, objects)
% 2. ACTION SPECIFICATION - Declares what operations are possible (move, pickup, etc.)
% 3. PHYSICS ENGINE - Rules for when actions work and how they change the world
% 4. REASONING ENGINE - Search algorithm that finds sequences of actions
% 5. USER INTERFACE - Utilities to display plans and execute them step-by-step
% 6. APPLICATION LAYER - Pre-configured examples and problem instances
%
% State Representation: state(RobotLocation, BoxLocation, RobotHolding)
% where RobotHolding is either 'nothing' or 'box'
% ============================================================================

% ============================================================================
% 1. DOMAIN MODEL (World Description)
% ============================================================================
% Basic facts about the world - the fundamental entities that exist

% Define possible locations
location(room1).
location(room2).
location(room3).

% ============================================================================
% 2. ACTION SPECIFICATION (What Can Happen)
% ============================================================================
% Abstract action templates - the vocabulary of possible operations

% Define possible actions
action(move(From, To)) :- 
    location(From), 
    location(To), 
    From \= To.

action(pickup(Location)) :- 
    location(Location).

action(putdown(Location)) :- 
    location(Location).

% ============================================================================
% 3. PHYSICS ENGINE (Action Semantics)
% ============================================================================
% World dynamics - rules that govern how actions interact with states
% This is like the physics of our virtual world

% Action preconditions and effects
% Move: robot must be at From location and not holding anything heavy
can_do(move(From, To), state(From, BoxLoc, nothing)) :-
    location(From),
    location(To),
    From \= To.

% Can also move while holding a box (robot is strong enough)
can_do(move(From, To), state(From, BoxLoc, box)) :-
    location(From),
    location(To),
    From \= To.

% Pickup: robot and box must be at same location, robot holding nothing
can_do(pickup(Loc), state(Loc, Loc, nothing)) :-
    location(Loc).

% Putdown: robot must be holding the box
can_do(putdown(Loc), state(Loc, _, box)) :-
    location(Loc).

% State transitions after actions
% Moving changes robot location, box moves with robot if being held
result(move(From, To), state(From, BoxLoc, nothing), state(To, BoxLoc, nothing)).
result(move(From, To), state(From, _, box), state(To, To, box)).

% Pickup changes what robot is holding and confirms box location
result(pickup(Loc), state(Loc, Loc, nothing), state(Loc, Loc, box)).

% Putdown changes what robot is holding, box stays at current location
result(putdown(Loc), state(Loc, _, box), state(Loc, Loc, nothing)).

% ============================================================================
% 4. REASONING ENGINE (Search Algorithm)
% ============================================================================
% Problem-solving algorithm - explores possible action sequences to find plans
% Uses depth-first search with cycle detection

% Planning predicate - finds sequence of actions to reach goal
% Uses visited states list to prevent cycles
plan(StartState, GoalState, Plan) :-
    plan(StartState, GoalState, [StartState], Plan).

% Base case: already at goal
plan(State, State, _, []).

% Recursive case: try an action and continue planning
plan(CurrentState, GoalState, Visited, [Action|RestPlan]) :-
    can_do(Action, CurrentState),
    result(Action, CurrentState, NextState),
    \+ member(NextState, Visited),  % Don't revisit states
    plan(NextState, GoalState, [NextState|Visited], RestPlan).

% ============================================================================
% 5. USER INTERFACE (Presentation Layer)
% ============================================================================
% Input/output and user experience - makes results human-readable

% Helper predicate to execute and show plan steps
execute_plan([], _, Step, Step).
execute_plan([Action|RestActions], CurrentState, StepNum, FinalStep) :-
    result(Action, CurrentState, NextState),
    NextStepNum is StepNum + 1,
    format('Step ~w: ~w~n', [StepNum, Action]),
    format('  State: ~w~n', [NextState]),
    execute_plan(RestActions, NextState, NextStepNum, FinalStep).

% Convenience predicate to plan and show execution
solve_problem(StartState, GoalState) :-
    plan(StartState, GoalState, Actions),
    format('Initial state: ~w~n', [StartState]),
    format('Goal state: ~w~n', [GoalState]),
    format('Plan found with ~w steps:~n', [Actions]),
    nl,
    execute_plan(Actions, StartState, 1, _).

% ============================================================================
% 6. APPLICATION LAYER (Specific Use Cases)
% ============================================================================
% Concrete applications - pre-configured problem instances and examples

% Example queries to try:
% ?- solve_problem(state(room1, room2, nothing), state(room1, room1, nothing)).
% ?- solve_problem(state(room1, room1, nothing), state(room3, room3, nothing)).
% ?- solve_problem(state(room1, room2, nothing), state(room3, room3, box)).

% More complex example - move box from room1 to room3
example1 :-
    solve_problem(state(room1, room1, nothing), state(room3, room3, nothing)).

% Example where robot starts away from box
example2 :-
    solve_problem(state(room1, room2, nothing), state(room3, room3, nothing)).

% Example with specific holding requirement
example3 :-
    solve_problem(state(room1, room1, nothing), state(room2, room2, box)).