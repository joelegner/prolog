natural_number(N) :-
    integer(N),
    N > 0.

whole_number(N) :-
    integer(N),
    N >= 0.

:- begin_tests(numbers).

% Tests for whole_number/1
test(whole_number_positive_24) :-
    whole_number(24).

test(whole_number_positive_4) :-
    whole_number(4).

test(whole_number_large_positive) :-
    whole_number(404301403204301240302140320140310234).

test(whole_number_zero) :-
    whole_number(0).

test(whole_number_negative, [fail]) :-
    whole_number(-1).

% Tests for natural_number/1
test(natural_number_zero, [fail]) :-
    natural_number(0).

test(natural_number_one) :-
    natural_number(1).

test(natural_number_positive) :-
    natural_number(432).

test(natural_number_negative, [fail]) :-
    natural_number(-432).

:- end_tests(numbers).
