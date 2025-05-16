#!/usr/bin/env swipl
% prologscript.pl
%
% Usage: ./prologscript.pl "2 + 2 * 3"
% Prints the result of the arithmetic expression.
%
% If no argument is given or evaluation fails, prints usage instructions.

:- initialization(main, main).

main(Argv) :-
    (   Argv = []
    ->  print_usage
    ;   atomic_list_concat(Argv, ' ', SingleArg),
        catch((
            term_to_atom(Term, SingleArg),
            Val is Term,
            format('~w~n', [Val])
        ), _, print_usage)
    ).

print_usage :-
    format('Usage:   ./prologscript.pl "EXPRESSION"~n'),
    format('Example: ./prologscript.pl "2 + 3 * 4"~n'),
    format('Note:    Expression must be in quotes.').
