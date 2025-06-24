% live_another_day.pl
live_another_day(
    be_awake(
        rest,
        play,
        work
    ),
    asleep
).

minutes(live_another_day, 1440).
activity_weight(be_awake, 3).
activity_weight(sleep, 1).
activity_weight(rest, 1).
activity_weight(play, 1).
activity_weight(work, 3).

node_activity_weight(Node, Weight) :-
    activity_weight(Node, Weight), !.
node_activity_weight(_, 1).

distribute_minutes(Minutes, Nodes, Result) :-
    maplist(node_activity_weight, Nodes, Weights),
    sum_list(Weights, TotalWeight),
    maplist(divvy(Minutes, TotalWeight), Nodes, Weights, Result).

divvy(Minutes, TotalWeight, Node, Weight, node_minutes(Node, Allocated)) :-
    Allocated is Minutes * Weight / TotalWeight.

report(Report) :-
    minutes(live_another_day, Mins),
    distribute_minutes(Mins, [be_awake, sleep], Report).

node_minutes(Node, Minutes) :-
    minutes(Node, Minutes), !.   % directly assigned
node_minutes(Node, Minutes) :-
    parent_minutes(Node, ParentMinutes),
    sibling_activity_weights(Node, TotalWeight, NodeWeight),
    Minutes is ParentMinutes * NodeWeight / TotalWeight.

activity_children(live_another_day, [be_awake, sleep]).
activity_children(be_awake, [rest, play, work]).
activity_children(sleep, []).
activity_children(rest, []).
activity_children(play, []).
activity_children(work, []).

parent(Child, Parent) :-
    activity_children(Parent, Children),
    member(Child, Children).

parent_minutes(Child, ParentMinutes) :-
    parent(Child, Parent),
    evaluate(Parent, ParentMinutes).

sibling_activity_weights(Node, TotalWeight, NodeWeight) :-
    parent(Node, Parent),
    activity_children(Parent, Siblings),
    maplist(node_activity_weight, Siblings, Weights),
    sum_list(Weights, TotalWeight),
    node_activity_weight(Node, NodeWeight).

evaluate(Node, Minutes) :-
    node_minutes(Node, Minutes).
