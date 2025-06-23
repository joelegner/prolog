%% kth.pl
contains([H|_], H).
contains([_|T], X) :- contains(T, X).

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