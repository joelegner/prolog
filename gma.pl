% gma.pl
% General Morphological Analysis

% General predicate to check if two attributes conflict using a constraint predicate
conflict(Eliminates, X, Y) :-
    call(Eliminates, X, Y);
    call(Eliminates, Y, X).

% Check for conflicts in a list of attributes using an eliminates predicate conflicts(_, 
% []) :- fail.
conflicts(_, [_]) :- fail.
conflicts(Eliminates, [X|Xs]) :-
    member(Y, Xs),
    conflict(Eliminates, X, Y), !.
conflicts(Eliminates, [_|Xs]) :-
    conflicts(Eliminates, Xs).

% True if the config has no conflicts according to a given eliminates predicate 
% Config is expected to be a list of value terms.
valid_config(Eliminates, Config) :-
    maplist(ground, Config),
    \+ conflicts(Eliminates, Config).

% Working example: 

/*
% gma_family.pl
% Family-inspired General Morphological Analysis example
% :- consult('gma.pl').

% Parameters joe_julie_home(walsingham).
joe_julie_home(ottawa).
joe_julie_home(rv).
joe_julie_home(apartment).

joey_home(walsingham).
joey_home(ottawa).
joey_home(apartment).

tommy_home(walsingham).
tommy_home(apartment).

sold(walsingham).
sold(ottawa).
sold(neither).
sold(both).

% Constraints eliminates_value(sold(Home), joe_julie_home(Home)).
eliminates_value(sold(Home), joey_home(Home)).
eliminates_value(sold(Home), tommy_home(Home)).

% Selling both houses elimiates them as anyone's home
eliminates_value(sold(both), joe_julie_home(walsingham)).
eliminates_value(sold(both), joe_julie_home(ottawa)).
eliminates_value(sold(both), tommy_home(walsingham)).
eliminates_value(sold(both), tommy_home(ottawa)).
eliminates_value(sold(both), joey_home(walsingham)).
eliminates_value(sold(both), joey_home(ottawa)).

valid_family_config(JOE_JULIE_HOME, JOEY_HOME, TOMMY_HOME, SOLD) :-
    joe_julie_home(JOE_JULIE_HOME),
    joey_home(JOEY_HOME),
    tommy_home(TOMMY_HOME),
    sold(SOLD),
    valid_config(eliminates_value, [joe_julie_home(JOE_JULIE_HOME), joey_home(JOEY_HOME), tommy_home(TOMMY_HOME), sold(SOLD)]).
    
*/
