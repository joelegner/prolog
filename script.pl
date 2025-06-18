#!/usr/bin/env swipl
% ^^^ This here shebang has to be the first line for it to work right.
% script.pl
% This is the only one you run from the command line.
% ./script.pl

:- initialization(main, main).

main(Argv) :-
    writeln('This simple script just echos argv.'),
    echo(Argv).

echo([]) :- nl.
echo([Last]) :- !,
    write(Last), nl.
echo([H|T]) :-
    write(H), write(' '),
    echo(T).
