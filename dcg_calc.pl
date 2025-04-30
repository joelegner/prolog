% dcg_calc.pl
% Provides DCG rules like number//1, blanks//0
:- use_module(library(dcg/basics)).    
% For unit testing:
:- use_module(library(plunit)).        

% Parses a simple arithmetic expression like "2+3" into a result.
parse(String, Result) :-
    string_codes(String, Codes),        % Convert string to list of character codes
    phrase(expr(Result), Codes).        % Parse codes using the DCG grammar

% --- How parsing works ---
%
% ?- parse("2+3", Result).
%
% Step-by-step:
%
% 1. "2+3" is a Prolog string. `string_codes/2` turns it into character codes:
%    [50,43,51] = ['2','+','3']
%
% 2. `phrase/2` runs the DCG rule `expr//1` on this list.
%
% 3. The rule:
%
%       expr(Result) -->
%           trimmed_integer(A),   % parses a number (with optional spaces)
%           "+",                  % matches the '+' character
%           trimmed_integer(B),   % parses another number
%           { Result is A + B }.  % evaluates and returns the sum
%
% Notes:
% - `trimmed_integer//1` ensures only valid digit sequences are parsed.
% - It uses `number//1` internally, which recognizes digit character codes.
% - The `{ ... }` block runs normal Prolog code after parsing.

% Parses an integer, allowing surrounding whitespace.
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
