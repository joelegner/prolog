% length_tests.pl
:- use_module(length).

:- begin_tests(length).

% Helper to capture output
output_of(Goal, Output) :-
    with_output_to(string(Output), Goal).

% Test cases
test(feet_0) :-
    output_of(format_length(feet(6), 0), "6 ft\n").

test(feet_default) :-
    output_of(format_length(feet(6)), "6.000 ft\n").

test(inches_negative_2) :-
    output_of(format_length(inches(-4.375), 2), "-4.38 in\n").

test(inches_default) :-
    output_of(format_length(inches(-6.875)), "-6.875 in\n").

test(mm_1digit) :-
    output_of(format_length(millimeters(42.42), 1), "42.4 mm\n").

test(cm_2digits) :-
    output_of(format_length(centimeters(42.42), 2), "42.42 cm\n").

test(meters_5digits) :-
    output_of(format_length(meters(1.2345), 5), "1.23450 m\n").

test(km_6digits) :-
    output_of(format_length(kilometers(0.75), 6), "0.750000 km\n").

test(miles_2digits) :-
    output_of(format_length(miles(1.5), 2), "1.50 mi\n").

:- end_tests(length).
