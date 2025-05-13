% axiology.pl
% File: formal_axiology.pl
% A Prolog model of value as conformity to the intension of a concept.
:- module(formal_axiology, [
    intension/2,
    entity/2,
    match_score/3,
    evaluate/2,
    run_tests/0
]).

:- use_module(library(lists)).
:- use_module(library(plunit)).

% ----------------------
% Concept intensions
% ----------------------
intension(chair, [has_legs, has_back, is_stable, is_sittable]).
intension(good_chocolate_cake, [is_chocolate, is_moist, is_sweet, has_good_texture]).
intension(tool, [is_functional, is_durable, fits_purpose]).

% ----------------------
% Objects and their properties
% ----------------------
entity(my_chair, [has_legs, has_back, is_sittable]).
entity(cake1, [is_chocolate, is_sweet, has_good_texture]).
entity(hammer, [is_functional, is_durable, fits_purpose]).
entity(broken_tool, [is_functional]).

exemplifies(my_chair, chair).
exemplifies(cake1, good_chocolate_cake).
exemplifies(hammer, tool).
exemplifies(hammer, tool).

% ----------------------
% Value scoring
% ----------------------
match_score(Concept, Entity, Score) :-
    intension(Concept, IntensionProps),
    entity(Entity, ActualProps),
    intersection(IntensionProps, ActualProps, Matching),
    length(IntensionProps, Total),
    length(Matching, Matches),
    Score is Matches / Total.

evaluate(Entity, Value) :-
    exemplifies(Entity, Concept),
    match_score(Concept, Entity, Value).

% ----------------------
% Unit tests
% ----------------------

:- begin_tests(formal_axiology).

test(my_chair_score) :-
    match_score(chair, my_chair, Score),
    Score =:= 0.75.

test(cake1_score) :-
    match_score(good_chocolate_cake, cake1, Score),
    Score =:= 0.75.

test(cake1_value) :-
    evaluate(cake1, Value),
    Value =:= 0.75.

test(hammer_score) :-
    match_score(tool, hammer, Score),
    Score =:= 1.0.

test(broken_tool_score) :-
    match_score(tool, broken_tool, Score),
    Score =:= 1/3.

:- end_tests(formal_axiology).

% ----------------------
% Run tests from top-level
% ----------------------