% gma.pl
% General Morphological Analysis
% --- General predicate to check if two attributes conflict using a constraint predicate ---
conflict(Eliminates, X, Y) :-
    call(Eliminates, X, Y);
    call(Eliminates, Y, X).

% --- Check for conflicts in a list of attributes using an eliminates predicate ---
conflicts(_, []) :- fail.
conflicts(_, [_]) :- fail.
conflicts(Eliminates, [X|Xs]) :-
    member(Y, Xs),
    conflict(Eliminates, X, Y), !.
conflicts(Eliminates, [_|Xs]) :-
    conflicts(Eliminates, Xs).

% --- True if the config has no conflicts according to a given eliminates predicate ---
valid_config(Eliminates, Config) :-
    \+ conflicts(Eliminates, Config).
