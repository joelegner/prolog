% length.pl
% Length reporting predicates
:- module(length, [
    write_length/1,
    write_length/2
]).

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
write_length(feet(N))        :- write_length(feet(N), 3).
write_length(inches(N))      :- write_length(inches(N), 3).
write_length(miles(N))       :- write_length(miles(N), 3).
write_length(millimeters(N)) :- write_length(millimeters(N), 3).
write_length(centimeters(N)) :- write_length(centimeters(N), 3).
write_length(meters(N))      :- write_length(meters(N), 3).
write_length(kilometers(N))  :- write_length(kilometers(N), 3).
