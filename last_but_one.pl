%% last_but_one.pl
/*
Find the second to last.
*/

last_but_one(List, El) :-
    writeln(List),
    reverse(List, Reversed),
    Reversed = [_|Rest],
    [El|_] = Rest.
