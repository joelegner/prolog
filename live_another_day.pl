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