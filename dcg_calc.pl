:- use_module(library(dcg/basics)).
:- use_module(library(plunit)).

% Entry point to parse a simple expression like "2+3"
parse(String, Result) :-
    string_codes(String, Codes),
    phrase(expr(Result), Codes).

% Grammar rule for expressions of the form A + B
expr(Result) --> 
    trimmed_integer(A),
    "+",
    trimmed_integer(B),
    { Result is A + B }.

% Integer parser with optional surrounding whitespace
trimmed_integer(N) --> blanks, number(N), blanks.

% Unit tests
:- begin_tests(parser).

test(simple_addition) :-
    parse("2+3", Result),
    assertion(Result == 5).

test(with_spaces) :-
    parse("  10 +  42 ", Result),
    assertion(Result == 52).

test(leading_and_trailing_spaces) :-
    parse("   7+8   ", Result),
    assertion(Result == 15).

:- end_tests(parser).
