%% units.pl

feet_inches(F, I) :-
    ground(F),
    ground(I),
    I =:= F * 12, !.

feet_inches(F, I) :-
    ground(I),
    \+ ground(F),
    must_be(number, I),
    F is I / 12, !.

feet_inches(F, I) :-
    ground(F),
    \+ ground(I),
    must_be(number, F),
    I is F * 12, !.

% Run the following like this:
% ?- run_tests.
:- begin_tests(units).

test(feet_to_inches) :-
    feet_inches(12, Inches),
    Inches =:= 144.

test(negative_decimal_feet_to_inches) :-
    feet_inches(-10.5, Inches),
    Inches =:= -126.0.

test(negative_decimal_inches_to_feet) :-
    feet_inches(Feet, -44.5),
    Feet =:= -3.7083333333333335.

test(integer_inches_to_feet) :-
    feet_inches(Feet, 48),
    Feet =:= 4.

test(zero_feet) :-
    feet_inches(0, Inches),
    Inches =:= 0.

test(zero_inches) :-
    feet_inches(Feet, 0),
    Feet =:= 0.

test(float_inches_from_int_feet) :-
    feet_inches(5, Inches),
    Inches =:= 60.0.

test(float_feet_from_float_inches) :-
    feet_inches(Feet, 66.0),
    Feet =:= 5.5.

test(round_trip_feet_inches) :-
    feet_inches(6.25, Inches),
    feet_inches(F2, Inches),
    F2 =:= 6.25.

test(round_trip_inches_feet) :-
    feet_inches(Feet, 99.9),
    feet_inches(Feet, I2),
    I2 =:= 99.9.

test(both_unbound_fails, [fail]) :-
    feet_inches(_, _).

test(non_number_input_1, [throws(error(type_error(number, hello), _))]) :-
    feet_inches(hello, _).

test(non_number_input_2, [throws(error(type_error(number, world), _))]) :-
    feet_inches(_, world).

:- end_tests(units).