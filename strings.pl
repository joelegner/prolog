%% strings.pl
/*
Usage example:
?- test('Joe Legner').
Dear Joe Legner,

This is an example of string interpolation using library(strings).
*/

:- use_module(library(strings)).

test(To) :-
    write({|string(To)||
           | Dear {To},
           |
           | This is an example of string interpolation using library(strings).
           |}).

/*
The code above seems to get rewritten to this by the library:

test(To) :-
    '.'(strings{type:string},
        exec([ "Dear ",
               var('To'),
               ",\n\nThis is an example of string interpolation using library(strings).\n"
             ],
             ['To'=To]),
        A),
    write(A).

We could make it live code like this.
*/

test_(To) :-
    '.'(strings{type:string},
        exec([ "Dear ",
               var('To'),
               ",\n\nThis is an example of string interpolation using library(strings).\n"
             ],
             ['To'=To]),
        A),
    write(A).

