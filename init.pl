% init.pl
% SWI Prolog startup file

/*
The other options for set_prolog_flag include:
codes
atom
chars
string
*/

:- set_prolog_flag(double_quotes, string).

/*
We will use the clpfd module by default. Here is the rationale.

https://www.swi-prolog.org/pldoc/man?section=clpfd-integer-arith

    "The arithmetic constraints (section A.9.2) #=/2, #>/2 etc. are meant to be used instead of the primitives (is)/2, (=:=)/2, (>)/2 etc. over integers. Almost all Prolog programs also reason about integers. Therefore, it is recommended that you put the following directive in your <config>/init.pl initialisation file to make CLP(FD) constraints available in all your programs:

    :- use_module(library(clpfd)).

    Throughout the following, it is assumed that you have done this."

The only other way to do this would be to put :- use_module(library(clpfd) in separate modules. Instead of this, I went ahead and added the following:
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