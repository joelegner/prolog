:- consult('gma.pl').

% --- Parameters ---
propulsion(electric).
propulsion(gas).
propulsion(hybrid).

terrain(road).
terrain(offroad).

size(small).
size(medium).
size(large).

% --- Constraints ---
eliminates_vehicle(size(large), propulsion(electric)).
eliminates_vehicle(size(small), terrain(offroad)).

% --- Convenience wrapper for the vehicle problem ---
valid_vehicle_config(P, T, S) :-
    propulsion(P),
    terrain(T),
    size(S),
    valid_config(eliminates_vehicle, [propulsion(P), terrain(T), size(S)]).
