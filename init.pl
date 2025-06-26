% init.pl
% SWI Prolog startup file

:- set_prolog_flag(double_quotes, string).
/*
The other options for set_prolog_flag include:
codes
atom
chars
string
*/

:- use_module(library(clpfd)).

% Clear the screen
c :-
    shell("clear").

date :-
    shell("date").

/*
List files:
?- ls.

List files that match wildcard:
?- ls('*.txt').
*/