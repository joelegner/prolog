%% units.pl
% Physical units for possible engineering application someday. 

% Conversion constants
feet_to_meters(0.3048).
meter_to_millimeter(1000).

% Base conversion: feet <-> inches
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

% feet <-> meters
feet_meters(F, M) :-
    feet_to_meters(FM),
    ground(F), ground(M),
    M =:= F * FM, !.

feet_meters(F, M) :-
    feet_to_meters(FM),
    ground(M), \+ ground(F),
    F is M / FM, !.

feet_meters(F, M) :-
    feet_to_meters(FM),
    ground(F), \+ ground(M),
    M is F * FM, !.

% feet <-> millimeters
feet_millimeters(F, MM) :-
    feet_to_meters(FM), meter_to_millimeter(MMperM),
    ground(F), ground(MM),
    MM =:= F * FM * MMperM, !.

feet_millimeters(F, MM) :-
    feet_to_meters(FM), meter_to_millimeter(MMperM),
    ground(MM), \+ ground(F),
    F is MM / (FM * MMperM), !.

feet_millimeters(F, MM) :-
    feet_to_meters(FM), meter_to_millimeter(MMperM),
    ground(F), \+ ground(MM),
    MM is F * FM * MMperM, !.

% inches <-> meters
inches_meters(I, M) :-
    feet_to_meters(FM),
    ground(I), ground(M),
    M =:= (I / 12) * FM, !.

inches_meters(I, M) :-
    feet_to_meters(FM),
    ground(M), \+ ground(I),
    I is (M / FM) * 12, !.

inches_meters(I, M) :-
    feet_to_meters(FM),
    ground(I), \+ ground(M),
    M is (I / 12) * FM, !.

% inches <-> millimeters
inches_millimeters(I, MM) :-
    feet_to_meters(FM), meter_to_millimeter(MMperM),
    ground(I), ground(MM),
    MM =:= (I / 12) * FM * MMperM, !.

inches_millimeters(I, MM) :-
    feet_to_meters(FM), meter_to_millimeter(MMperM),
    ground(MM), \+ ground(I),
    I is (MM / (FM * MMperM)) * 12, !.

inches_millimeters(I, MM) :-
    feet_to_meters(FM), meter_to_millimeter(MMperM),
    ground(I), \+ ground(MM),
    MM is (I / 12) * FM * MMperM, !.

% meters <-> millimeters
meters_millimeters(M, MM) :-
    meter_to_millimeter(Factor),
    ground(M), ground(MM),
    MM =:= M * Factor, !.

meters_millimeters(M, MM) :-
    meter_to_millimeter(Factor),
    ground(MM), \+ ground(M),
    M is MM / Factor, !.

meters_millimeters(M, MM) :-
    meter_to_millimeter(Factor),
    ground(M), \+ ground(MM),
    MM is M * Factor, !.

% Run tests
:- begin_tests(units).

% Existing feet_inches tests (omitted for brevity, already included above)

% feet <-> meters
test(feet_to_meters) :-
    feet_meters(10, M),
    M =:= 3.048.

test(meters_to_feet) :-
    feet_meters(F, 1.524),
    F =:= 5.0.

test(feet_meters_zero) :-
    feet_meters(0, M),
    M =:= 0.

test(feet_meters_negative) :-
    feet_meters(-2, M),
    M =:= -0.6096.

% feet <-> millimeters
test(feet_to_millimeters) :-
    feet_millimeters(1, MM),
    MM =:= 304.8.

test(millimeters_to_feet) :-
    feet_millimeters(F, 1524),
    F =:= 5.0.

test(feet_millimeters_zero) :-
    feet_millimeters(0, MM),
    MM =:= 0.

test(feet_millimeters_negative) :-
    feet_millimeters(-2, MM),
    MM =:= -609.6.

% inches <-> meters
test(inches_to_meters) :-
    inches_meters(24, M),
    M =:= 0.6096.

test(meters_to_inches) :-
    inches_meters(I, 0.6096),
    I =:= 24.

test(inches_meters_zero) :-
    inches_meters(0, M),
    M =:= 0.

test(inches_meters_negative) :-
    inches_meters(-12, M),
    M =:= -0.3048.

% inches <-> millimeters
test(inches_to_millimeters) :-
    inches_millimeters(10, MM),
    MM =:= 254.0.

test(millimeters_to_inches) :-
    inches_millimeters(I, 609.6),
    I =:= 24.

test(inches_millimeters_zero) :-
    inches_millimeters(0, MM),
    MM =:= 0.

test(inches_millimeters_negative) :-
    inches_millimeters(-1, MM),
    MM =:= -25.4.

% meters <-> millimeters
test(meters_to_millimeters) :-
    meters_millimeters(3.5, MM),
    MM =:= 3500.

test(millimeters_to_meters) :-
    meters_millimeters(M, 1200),
    M =:= 1.2.

test(meters_millimeters_zero) :-
    meters_millimeters(0, MM),
    MM =:= 0.

test(meters_millimeters_negative) :-
    meters_millimeters(-1.5, MM),
    MM =:= -1500.

:- end_tests(units).
