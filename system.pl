% system.pl
% Let's declare a flashlight system with subsystem parts.
% Template:
% system(name, function, [part1, part2, ..., part_n]).
system(flashlight, illuminate_area, [head, body]).
system(body, contain_battery, [cylinder, battery]).
system(head, make_light, [lens, bulb, housing]).
system(bulb, convert_elec_to_light, []).
system(battery, supply_voltage, []).

% system/1 checks to see if X is a system.
% It gets this from the structures above.
system(X) :- system(X, _, _).

% A subsystem is a system that is part of a larger system.
% X is the system being unified.
% P is a list of parts.
subsystem(X) :- 
    system(X, _, _),
    system(_, _, P),
    member(X, P).

% A system with parts is an assembly
% A system with a non-empty parts list is an assembly.
assembly(X) :-
    system(X, _, [_|_]).

% A system with zero parts is an assembly.
% An assembly has an empty parts list.
%  X = the system being checked.
% [] = standard Prolog for "empty list".
%  _ = standard Prolog for "anonymous variable".
% I wrote this totally on my own and am proud of it. :-)
unit(X) :-
    system(X, _, []).
