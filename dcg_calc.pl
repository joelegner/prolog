:- use_module(library(dcg/basics)).
:- use_module(library(plunit)).

% Entry point to parse a simple expression like "2+3"
parse(String, Result) :-
    string_codes(String, Codes),
    phrase(expr(Result), Codes).

% How does this work?
% ?- parse("2+3", Result).
% 
% Look at our parse predicate:
%
% parse(String, Result) :-
%     string_codes(String, Codes),
%     phrase(expr(Result), Codes).
%
% String = "2+3" — a normal Prolog string. Given in the query.
% string_codes(String, Codes) turns "2+3" into a list of character codes:
% Codes = [50,43,51]  % ASCII for '2', '+', '3'
% phrase(expr(Result), Codes) runs the DCG expr//1 on the character code list.
%
% Now we turn our attention to the DCG rule.
%
% expr(Result) --> 
%     integer(A),     % parse first number
%     "+",            % match plus sign
%     integer(B),     % parse second number
%     { Result is A + B }.  % evaluate result
%
% Notes:
% 1. integer(A) consumes codes for the first number and binds A.
% 2. "+" matches the character code 43 (the plus sign).
% 3. integer(B) consumes the second number and binds B.
% 4. { Result is A + B } computes and returns the result.

% Integer parser with optional surrounding whitespace.
% This is the part that ensures A and B in expr/1 below are numbers.
trimmed_integer(N) --> blanks, number(N), blanks.

% Grammar rule for expressions of the form A + B
expr(Result) --> 
    trimmed_integer(A),
    "+",
    trimmed_integer(B),
    { Result is A + B }.


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
