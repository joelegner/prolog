% length.pl
% Demonstrates handling lengths and writing them.

% Imperial units
write_length(feet(N), Digits) :-
    format('~*f ft~n', [Digits, N]).

write_length(inches(N), Digits) :-
    format('~*f in~n', [Digits, N]).

write_length(miles(N), Digits) :-
    format('~*f mi~n', [Digits, N]).

% SI units
write_length(millimeters(N), Digits) :-
    format('~*f mm~n', [Digits, N]).

write_length(centimeters(N), Digits) :-
    format('~*f cm~n', [Digits, N]).

write_length(meters(N), Digits) :-
    format('~*f m~n', [Digits, N]).

write_length(kilometers(N), Digits) :-
    format('~*f km~n', [Digits, N]).

% Default to 3 digits
write_length(feet(N)) :- 
    write_length(feet(N), 3).

write_length(inches(N)) :- 
    write_length(inches(N), 3).

write_length(miles(N)) :- 
    write_length(miles(N), 3).

write_length(millimeters(N)) :- 
    write_length(millimeters(N), 3).

write_length(centimeters(N)) :- 
    write_length(centimeters(N), 3).

write_length(meters(N)) :- 
    write_length(meters(N), 3).

write_length(kilometers(N)) :- 
    write_length(kilometers(N), 3).

run :-
    write_length(feet(6), 0),
    write_length(feet(6), 1),
    write_length(feet(6), 2),
    write_length(feet(6)),
    write_length(feet(6), 4),

    write_length(inches(-4.375), 0),
    write_length(inches(-4.375), 1),
    write_length(inches(-4.375), 2),
    write_length(inches(-4.375)),
    write_length(inches(-6.875), 0),
    write_length(inches(-6.875), 1),
    write_length(inches(-6.875), 2),
    write_length(inches(-6.875)),
    write_length(inches(-6.875), 4),

    write_length(millimeters(42.42)),
    write_length(millimeters(42.42), 1),

    write_length(centimeters(42.42)),
    write_length(centimeters(42.42), 2),

    write_length(meters(1.2345)),
    write_length(meters(1.2345), 5),

    write_length(kilometers(0.75)),
    write_length(kilometers(0.75), 6),

    write_length(miles(1.5)),
    write_length(miles(1.5), 2).
