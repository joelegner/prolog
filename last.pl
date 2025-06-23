%% last.pl
/*
Let's get the last element in a list.

?- last([a,b,c,d,e], L).
L = e.
*/

last(List, El) :-
    reverse(List, [El|_]).

/*
The reverse call does a lot of work. First, it generates a reverse list. but that reverse list immediately gets sliced up by the [El|_] syntax which says, "unify the element El with the first item in the (reverse) list." Thus, El gets unified with the first item in the reverse list which is the same thing as the last element in the forward list. 
*/