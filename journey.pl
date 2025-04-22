% J is a journey if:
% J has a start, S.
% J has a (current) position, P.
% J has an end, E.
% S, P, and E are all places.
journey(Start, Pos, End) :-
    place(Start),
    place(Pos),
    place(End),
    Start \= End.

place(chicago).
place(st_louis).
place(los_angeles).
