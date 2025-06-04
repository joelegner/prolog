% gma_family.pl
% Family-inspired General Morphological Analysis example
:- consult('gma.pl').

% --- Parameters ---
joe_julie_home(walsingham).
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
sold([]).
sold([walsingham, ottawa]).

% No apartment together with keeping Walsingham
eliminates_value(_, joe_julie_home(apartment)) :- \+ sold(walsingham).
eliminates_value(_, tommy_home(apartment)) :- \+ sold(walsingham).
eliminates_value(_, joey_home(apartment)) :- \+ sold(walsingham).

% Cross-consistency constraints
% Selecting the left value eliminates the right
eliminates_value(sold(Home), joe_julie_home(Home)).
eliminates_value(sold(Home), joey_home(Home)).
eliminates_value(sold(Home), tommy_home(Home)).

% Selling both houses elimiates them as anyone's home
eliminates_value(sold([walsingham, ottawa]), joe_julie_home(walsingham)).
eliminates_value(sold([walsingham, ottawa]), joe_julie_home(ottawa)).
eliminates_value(sold([walsingham, ottawa]), tommy_home(walsingham)).
eliminates_value(sold([walsingham, ottawa]), tommy_home(ottawa)).
eliminates_value(sold([walsingham, ottawa]), joey_home(walsingham)).
eliminates_value(sold([walsingham, ottawa]), joey_home(ottawa)).

valid_family_config([JOE_JULIE_HOME, JOEY_HOME, TOMMY_HOME, SOLD]) :-
    joe_julie_home(JOE_JULIE_HOME),
    joey_home(JOEY_HOME),
    tommy_home(TOMMY_HOME),
    sold(SOLD),
    valid_config(eliminates_value, [joe_julie_home(JOE_JULIE_HOME), joey_home(JOEY_HOME), tommy_home(TOMMY_HOME), sold(SOLD)]).

valid_family_config(Config) :-
    Config = [JOE_JULIE_HOME, JOEY_HOME, TOMMY_HOME, SOLD],
    maplist(ground, Config),
    joe_julie_home(JOE_JULIE_HOME),
    joey_home(JOEY_HOME),
    tommy_home(TOMMY_HOME),
    sold(SOLD),
    valid_config(eliminates_value, [
        joe_julie_home(JOE_JULIE_HOME),
        joey_home(JOEY_HOME),
        tommy_home(TOMMY_HOME),
        sold(SOLD)
    ]).

valid_family_configs(Configs) :-
    findall(
        [JOE_JULIE_HOME, JOEY_HOME, TOMMY_HOME, SOLD],
        valid_family_config([JOE_JULIE_HOME, JOEY_HOME, TOMMY_HOME, SOLD]),
        Configs
    ).
