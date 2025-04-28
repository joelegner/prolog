live_another_day(
    be_awake(
        rest,
        play,
        work
    ),
    asleep
).

minutes(live_another_day, 1440).
weight(be_awake, 3).
weight(sleep, 1).
weight(rest, 1).
weight(play, 1).
weight(work, 3).

get_weight(Node, Weight) :-
    weight(Node, Weight), !.
get_weight(_, 1).

distribute_minutes(Minutes, Nodes, Result) :-
    maplist(get_weight, Nodes, Weights),
    sum_list(Weights, TotalWeight),
    maplist(divvy(Minutes, TotalWeight), Nodes, Weights, Result).

divvy(Minutes, TotalWeight, Node, Weight, node_minutes(Node, Allocated)) :-
    Allocated is Minutes * Weight / TotalWeight.

report(Report) :-
    minutes(live_another_day, Mins),
    distribute_minutes(Mins, [be_awake, sleep], Report).

get_minutes(Node, Minutes) :-
    minutes(Node, Minutes), !.   % directly assigned
get_minutes(Node, Minutes) :-
    parent_minutes(Node, ParentMinutes),
    sibling_weights(Node, TotalWeight, NodeWeight),
    Minutes is ParentMinutes * NodeWeight / TotalWeight.

children(live_another_day, [be_awake, sleep]).
children(be_awake, [rest, play, work]).
children(sleep, []).
children(rest, []).
children(play, []).
children(work, []).

parent(Child, Parent) :-
    children(Parent, Children),
    member(Child, Children).

parent_minutes(Child, ParentMinutes) :-
    parent(Child, Parent),
    evaluate(Parent, ParentMinutes).

sibling_weights(Node, TotalWeight, NodeWeight) :-
    parent(Node, Parent),
    children(Parent, Siblings),
    maplist(get_weight, Siblings, Weights),
    sum_list(Weights, TotalWeight),
    get_weight(Node, NodeWeight).

evaluate(Node, Minutes) :-
    get_minutes(Node, Minutes).
