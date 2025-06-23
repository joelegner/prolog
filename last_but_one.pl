%% last_but_one.pl
/*
Find the second to last.
*/

last_but_one(List, El) :-
    reverse(List, [_|[El|_]]).

/* 
This is a fantastic solution. Look at that. The second term does the work in the reverse call. It discards the first character of the reversed list. That's important to note. If you think it is removing the first element of List, that is only half of what it is doing. First it is reversing List, then lopping off the head which is the very last character.

?- last_but_one([a,b,c,d,e], El).
El = d.

?- last_but_one([a,b,c,d], El).
El = c.

It works.
*/