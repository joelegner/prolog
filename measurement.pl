:- use_module(library(clpr)).

% millimeters: supports both directions
millimeters(Inches, inch, Millimeters) :-
    { Millimeters = 25.4 * Inches }.

millimeters(Feet, foot, Millimeters) :-
    { Millimeters = 12.0 * 25.4 * Feet }.

% meters: supports both directions
meters(Inches, inch, Meters) :-
    millimeters(Inches, inch, Millimeters),
    { Meters = Millimeters / 1000.0 }.

% inches: inch form (pass-through)
inches(Inches, inch, Inches).

% inches: convert from feet
inches(Feet, foot, Inches) :-
    Inches is 12 * Feet.

% main: simple test
main :-
    writeln("Starting Measurement.pl Prolog"),
    inches(12.0, foot, Inches1),
    writeln(Inches1),
    inches(12.0, inch, Inches2),
    writeln(Inches2),
    meters(Inches3, inch, 1.0),  % solve for Inches3
    format('1.0 meter is ~10f inches~n', [Inches3]),
    meters(39.3701, inch, Meters),  % solve for Meters
    format('39.3701 inches is ~10f meters~n', [Meters]).
