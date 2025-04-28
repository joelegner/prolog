system(flashlight, illuminate_area, [head, body]).
system(body, contain_battery, [cylinder, battery]).
system(head, make_light, [lens, bulb, housing]).
system(bulb, convert_elec_to_light, [enclosure, filament, connections]).
system(battery, supply_voltage, []).

% A subsystem is a system that is part of a larger system.
% X is the system being unified.
% P is a list of parts.
subsystem(X) :- 
    system(X, _, _),
    system(_, _, P),
    member(X, P).

not_empty_list([_|_]).

% A system with parts is an assembly
% A system with a non-empty parts list is an assembly.
% P is a list of parts
assembly(X) :-
    system(X, _, P),
    not_empty_list(P).

% A system with zero parts is an assembly.
% An assembly has an empty parts list.
% P is a list of parts.
unit(X) :-
    system(X, _, []).
