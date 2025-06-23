%% kth.pl
/*
This is a challenge to unify a variable with the kth element in a list.

This version works. It was written by ChatGPT.
*/
kth([H|_], 1, H).
kth([_|T], K, X) :-
    K > 1,
    K1 is K - 1,
    kth(T, K1, X).

/*
 ?- kth([a,b,c,d,e], 3, X).
   Call: (12) kth([a, b, c, d, e], 3, _22236) ? creep
   Call: (13) 3>1 ? creep
   Exit: (13) 3>1 ? creep
   Call: (13) _25242 is 3+ -1 ? creep
   Exit: (13) 2 is 3+ -1 ? creep
   Call: (13) kth([b, c, d, e], 2, _22236) ? creep
   Call: (14) 2>1 ? creep
   Exit: (14) 2>1 ? creep
   Call: (14) _29304 is 2+ -1 ? creep
   Exit: (14) 1 is 2+ -1 ? creep
   Call: (14) kth([c, d, e], 1, _22236) ? creep
   Exit: (14) kth([c, d, e], 1, c) ? creep
   Exit: (13) kth([b, c, d, e], 2, c) ? creep
   Exit: (12) kth([a, b, c, d, e], 3, c) ? creep
*/

/* 
How does this work? Let's try to figure it out together. Let's say we call kth/3 like this:

?- kth([a,b,c,d,e], 3, X).

It does not match the rule kth([H|_], 1, H), because K is not 1. 

It matches the rule kth([_|T], K, X), however. Let's unify the rule head with the query.

kth([_|T], 3, X) = kth([a,b,c,d,e], 3, X)

This means T = [b,c,d,e]. It also means K1 = 3 - 1 = 2. Now the rule calls kth/3 again with these new updated values.

Calling kth(T, K1, X) gives us kth([b,c,d,e], 2, X). We unify with the rule again.

kth([_|T], 3, X) = kth([b,c,d,e], 2, X)

T = [c,d,e] and K1 = 1. 

Calling kth(T, K1, X) gives us kth([c,d,e], 1, X). But wait a minute. This matches the rule kth([H|_], 1, H). We can finally bind the variable in the call to the head of the list represented by H in the clause. 

kth([H|_], 1, H) = kth([c|d,e], 1, c).

Success.
*/
