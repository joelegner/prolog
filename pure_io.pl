% pure_io.pl
% Reads lines from a file using pio and dcg/basics.

:- use_module(library(dcg/basics)).

... --> [] | [_], ... .

file -->
    match(Line),
    eol,
    { format('~s~n', [Line]) }.

match(Line) -->
        string_without("\n", Line),
        eol.

/*
?- phrase_from_file(file, 'pure_io.txt').
First line.
Second line.
Third line.
Fourth line.
Fifth line.
*/