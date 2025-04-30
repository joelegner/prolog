% philosophy.pl
:- use_module('axiomatic_design.pl').

:- multifile axiomatic_design:zig/2.
:- multifile axiomatic_design:zag/2.

% Note: if you leave off the :- the above line results in an error:
%
% use_module(library('axiomatic_design')).
% swipl -s philosophy.pl
% Welcome to SWI-Prolog (threaded, 64 bits, version 9.2.9)
% SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
% Please run ?- license. for legal details.

% For online help and background, visit https://www.swi-prolog.org
% For built-in help, use ?- help(Topic). or ?- apropos(Word).

% ?- all_need_to_zag(All).
% ERROR: Unknown procedure: all_need_to_zag/1 (DWIM could not correct goal)
%
% The way to FIX the error is to add the :- before use_module/1. See above.

% Local reminder of predicates in axiomatic_design:
% :- module(axiomatic_design, [
%     all_need_to_zag/1,
%     design_parameter/1,
%     funtional_requirement/1,
%     need_to_zag/1,
%     parent/2
% ]).

% Good is a quality independent of conditions.
goodness.

% Identify things as having goodness or not
has(health, goodness).
has(love, goodness).
has(care, goodness).
has(caring, goodness).
has(concern, goodness).
has(pleasure, goodness).

does_not_have(pain, goodness).
does_not_have(anxiety, goodness).
does_not_have(addiction, goodness).
does_not_have(harm, goodness).
does_not_have(hate, goodness).

is_a(self_harm, harm).

all_good(Goods) :-
     findall(Good, has(Good, goodness), Goods).

% Psychology
axiomatic_design:zig(live_examined_life, philosopher).
