% physics.pl
% Define what matter is
matter(X) :- has_mass(X), has_extension(X).

% Facts
has_mass(microphone).
has_mass(air).
% Note that we omitted light here

has_extension(microphone).
has_extension(air).
has_extension(light).

% Unit tests
:- begin_tests(matter_tests).

test(microphone_is_matter) :-
    matter(microphone).

test(air_is_matter) :-
    matter(air).

test(light_is_not_matter, [fail]) :-
    matter(light).

:- end_tests(matter_tests).
