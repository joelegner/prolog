brave(joe).
has_integrity(joe).
persistant(joe).
vital(joe).

couragous(Person) :- 
    brave(Person),
    has_integrity(Person),
    persistant(Person),
    vital(Person).

% ?- couragous(P).
% P = joe.
