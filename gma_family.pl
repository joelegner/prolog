% gma_family.pl
% Family-inspired General Morphological Analysis example
:- consult('gma.pl').

% # Dimensions and possible values

% dimension1(value1). 
% dimension2(value2). 

% Dimension 1
joe_julie_home(walsingham).
joe_julie_home(ottawa).
joe_julie_home(rv).
joe_julie_home(apartment).

% Dimension 2
joey_home(walsingham).
joey_home(ottawa).
joey_home(apartment).

% Dimension 3
tommy_home(walsingham).
tommy_home(apartment).

% Dimension 4
sold([walsingham]).
sold([ottawa]).
sold([]).
sold([walsingham, ottawa]).

% # Relations among dimensions

% List of all houses that can be sold
all_houses([walsingham, ottawa]).

% No apartment together with keeping Walsingham
% If anyone lives at Walsingham, nobody lives in an apartment
eliminates_value(joe_julie_home(walsingham), joey_home(apartment)).
eliminates_value(joe_julie_home(walsingham), tommy_home(apartment)).
eliminates_value(joey_home(walsingham), tommy_home(apartment)).
eliminates_value(joe_julie_home(apartment), joey_home(walsingham)).
eliminates_value(joe_julie_home(apartment), tommy_home(walsingham)).
eliminates_value(joey_home(apartment), tommy_home(walsingham)).

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

print_valid_family_configs :-
    valid_family_configs(Configs),
    print_configs(Configs, 1).

print_configs([], _).
print_configs([[JJ, JY, TM, SD]|Rest], N) :-
    all_houses(All),
    (   is_list(SD) -> SoldList = SD ; SoldList = [SD] ),
    subtract(All, SoldList, Kept),
    format("~nConfig ~d~n", [N]),
    format("  Joe & Julie: ~w~n", [JJ]),
    format("  Joey       : ~w~n", [JY]),
    format("  Tommy      : ~w~n", [TM]),
    format("  Kept       : ~w~n", [Kept]),
    format("  Sold       : ~w~n", [SD]),
    Next is N + 1,
    print_configs(Rest, Next).
