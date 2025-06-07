% measurement.pl
% Unit conversion without clpr

% millimeters_from_inches(+Inches, -Millimeters)
millimeters_from_inches(Inches, Millimeters) :-
    Millimeters is 25.4 * Inches.

% millimeters_from_feet(+Feet, -Millimeters)
millimeters_from_feet(Feet, Millimeters) :-
    Millimeters is 12.0 * 25.4 * Feet.

% meters_from_inches(+Inches, -Meters)
meters_from_inches(Inches, Meters) :-
    millimeters_from_inches(Inches, Millimeters),
    Meters is Millimeters / 1000.0.

% inches_from_meters(+Meters, -Inches)
inches_from_meters(Meters, Inches) :-
    Millimeters is Meters * 1000.0,
    Inches is Millimeters / 25.4.

% inches(+Value, +Unit, -Inches)
inches(Inches, inch, Inches).
inches(Feet, foot, Inches) :-
    Inches is 12 * Feet.

% inches_from_feet(+Feet, -Inches)
inches_from_feet(Feet, Inches) :-
    Inches is 12 * Feet.

% format_decimal_inches(+Inches)
format_decimal_inches(Inches) :-
    Inches =< 1.0,
    format('~3f inch~n', [Inches]).

format_decimal_inches(Inches) :-
    Inches > 1.0,
    format('~3f inches~n', [Inches]).

% main: simple test
main :-
    writeln("Starting Measurement.pl Prolog"),
    inches(12.0, foot, Inches1),
    writeln(Inches1),
    inches(12.0, inch, Inches2),
    writeln(Inches2),
    inches_from_meters(1.0, Inches3),
    format('1.0 meter is ~10f inches~n', [Inches3]),
    meters_from_inches(39.3701, Meters),
    format_decimal_inches(0.75),
    format_decimal_inches(1.875),
    format_decimal_inches(4),
    format('39.3701 inches is ~2f meters~n', [Meters]).
