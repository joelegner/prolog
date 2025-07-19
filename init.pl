% init.pl
% SWI Prolog startup file

/*
This comment is about the set_prolog_flag setting below.

:- set_prolog_flag(double_quotes, Flag).

Flag may be one of these:
codes
atom
chars
string

The flag `string` seems to work best. But to get it to display results when using phrase/3 requires use of another predicate: string_codes/2:

?- phrase(day_name(X), Phrase),  string_codes(String, Phrase).
X = 0,
Phrase = [83, 117, 110],
String = "Sun"
*/
:- set_prolog_flag(double_quotes, string).

/*
The setting below setting makes it so no results are truncated. This can make for long output. If you want truncated results in a particular consulted file, add this to it:

:- set_prolog_flag(answer_write_options, [quoted(true), portrayed(true), max_depth(10), spacing(next_argument)]).

Doing so will override this init.pl file flag. 
*/
:- set_prolog_flag(answer_write_options,[max_depth(0)]).

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
    shell('clear').

date :-
    shell('date').

/*
List files:
?- ls.

List files that match wildcard:
?- ls('*.txt').
*/