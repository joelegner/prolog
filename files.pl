:- use_module(library(files)).
:- use_module(library(dcg/basics)).

... --> [] | [_], ... .

photo_file_name --> ... ,".pl".

run :-
    directory_files(".", Fs),
    writeln(Fs).
