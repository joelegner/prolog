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
tommy_home(ottawa).
tommy_home(apartment).

sold(walsingham).
sold(ottawa).

% --- Constraints ---
eliminates_value(sold(walsingham), joe_julie_home(walsingham)).
eliminates_value(sold(walsingham), joey_home(walsingham)).
eliminates_value(sold(walsingham), tommy_home(walsingham)).

% --- Convenience wrapper for the vehicle problem ---
valid_family_config(JOE_JULIE_HOME, JOEY_HOME, TOMMY_HOME) :-
    joe_julie_home(JOE_JULIE_HOME),
    joey_home(JOEY_HOME),
    tommy_home(TOMMY_HOME),
    valid_config(eliminates_value, [joe_julie_home(JOE_JULIE_HOME), joey_home(JOEY_HOME), tommy_home(TOMMY_HOME)]).
    