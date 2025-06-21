#!/usr/bin/env swipl

:- initialization(main, main).

main([FileStem]) :-
    atom_concat(FileStem, '.pl', PLFile),

    (   exists_file(PLFile)
    ->  format("File ~w already exists.~n", [PLFile])
    ;   create_prolog_file_with_header(PLFile),
        format("Created ~w~n", [PLFile])
    ),

    append_to_makefile(FileStem, PLFile),
    format("Added ~w target to Makefile~n", [FileStem]),
    halt(0).

main(_) :-
    format("Usage: add_file.pl <filename (without extension)>~n"),
    halt(1).

create_prolog_file_with_header(PLFile) :-
    setup_call_cleanup(
        open(PLFile, write, Stream),
        format(Stream, "%% ~w~n", [PLFile]),
        close(Stream)).

append_to_makefile(Target, PLFile) :-
    format_string('.PHONY: ~w', [Target], Line1),
    format_string('~w:', [Target], Line2),
    format_string('\tswipl -s ~w', [PLFile], Line3),
    Lines = [
        '',
        Line1,
        Line2,
        Line3
    ],
    setup_call_cleanup(
        open('Makefile', append, Stream),
        write_lines(Stream, Lines),
        close(Stream)
    ).

write_lines(_, []).
write_lines(Stream, [Line|Rest]) :-
    format(Stream, "~w~n", [Line]),
    write_lines(Stream, Rest).

format_string(Format, Args, Atom) :-
    format(atom(Atom), Format, Args).
