% J is a journey if:
% J has a start, S.
% J has a (current) position, P.
% J has an end, E.
% S, P, and E are all places.
journey(J) :- 
    start(J, S),
    position(J, P),
    end(J, E),
    place(S),
    place(P),
    place(E).

place(chicago).
place(st_louis).
place(los_angeles).
