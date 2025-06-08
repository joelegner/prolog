% length.pl
% Length reporting predicates
:- module(length, [
    format_length/1,
    format_length/2
]).

% Imperial units
format_length(feet(N), Digits) :-
    format('~*f ft~n', [Digits, N]).
format_length(inches(N), Digits) :-
    format('~*f in~n', [Digits, N]).
format_length(miles(N), Digits) :-
    format('~*f mi~n', [Digits, N]).

% SI units
format_length(millimeters(N), Digits) :-
    format('~*f mm~n', [Digits, N]).
format_length(centimeters(N), Digits) :-
    format('~*f cm~n', [Digits, N]).
format_length(meters(N), Digits) :-
    format('~*f m~n', [Digits, N]).
format_length(kilometers(N), Digits) :-
    format('~*f km~n', [Digits, N]).

% Default to 3 digits
format_length(feet(N))        :- format_length(feet(N), 3).
format_length(inches(N))      :- format_length(inches(N), 3).
format_length(miles(N))       :- format_length(miles(N), 3).
format_length(millimeters(N)) :- format_length(millimeters(N), 3).
format_length(centimeters(N)) :- format_length(centimeters(N), 3).
format_length(meters(N))      :- format_length(meters(N), 3).
format_length(kilometers(N))  :- format_length(kilometers(N), 3).
