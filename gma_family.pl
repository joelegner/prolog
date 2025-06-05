% gma_family.pl
% Family-inspired General Morphological Analysis example
:- consult('gma.pl').

% # Dimensions and possible values

joe_julie_home_now(walsingham).
joey_home_now(ottawa).
tommy_home_now(walsingham).

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

% Constraints
eliminates_value(joe_julie_home(walsingham), joey_home(apartment)).
eliminates_value(joe_julie_home(walsingham), tommy_home(apartment)).
eliminates_value(joey_home(walsingham), tommy_home(apartment)).
eliminates_value(joe_julie_home(apartment), joey_home(walsingham)).
eliminates_value(joe_julie_home(apartment), tommy_home(walsingham)).
eliminates_value(joey_home(apartment), tommy_home(walsingham)).

eliminates_value(sold(Home), joe_julie_home(Home)).
eliminates_value(sold(Home), joey_home(Home)).
eliminates_value(sold(Home), tommy_home(Home)).

eliminates_value(sold([walsingham, ottawa]), joe_julie_home(walsingham)).
eliminates_value(sold([walsingham, ottawa]), joe_julie_home(ottawa)).
eliminates_value(sold([walsingham, ottawa]), tommy_home(walsingham)).
eliminates_value(sold([walsingham, ottawa]), tommy_home(ottawa)).
eliminates_value(sold([walsingham, ottawa]), joey_home(walsingham)).
eliminates_value(sold([walsingham, ottawa]), joey_home(ottawa)).

% Valid configuration check
valid_family_config([JOE_JULIE_HOME, JOEY_HOME, TOMMY_HOME, SOLD]) :-
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

% Return all valid configs
valid_family_configs(Configs) :-
    findall(
        [JOE_JULIE_HOME, JOEY_HOME, TOMMY_HOME, SOLD],
        valid_family_config([JOE_JULIE_HOME, JOEY_HOME, TOMMY_HOME, SOLD]),
        Configs
    ).

% Top-level entry point
print_valid_family_configs :-
    valid_family_configs(Configs),
    print_configs(Configs, 1).

% Print each config
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
    format("  Implications:~n"),

    % ✅ FIXED: Wrap sold items in sold/1 terms
    maplist(wrap_sold, SoldList, SoldTerms),

    ConfigFacts = [joe_julie_home(JJ), joey_home(JY), tommy_home(TM) | SoldTerms],
    print_implications(ConfigFacts),

    Next is N + 1,
    print_configs(Rest, Next).

% Wrap a sold item in sold/1 term
wrap_sold(House, sold(House)).

% Implications
implies(sold(walsingham), '$100K cash on hand').
implies(sold(walsingham), 'Greatly reduced expenses').
implies(sold(walsingham), 'Need much additional storage').
implies(sold(ottawa), '$10K cash on hand').
implies(sold(ottawa), 'Reduced expenses').

% Print implications for a config
print_implications([]).
print_implications([H|T]) :-
    findall(Implication, implies(H, Implication), Implied),
    print_implication_list(H, Implied),
    print_implications(T).

print_implication_list(_, []).
print_implication_list(Fact, [I|Rest]) :-
    format("    ~w ⇒ ~w~n", [Fact, I]),
    print_implication_list(Fact, Rest).
