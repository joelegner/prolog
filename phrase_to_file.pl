%% phrase_to_file.pl
:- module(phrase_to_file, [
    phrase_to_file/2,
    main/0,
    file//0
]).

:- use_module(phrase_string).

file --> 
    "Hello, World!".

main :-
    phrase_to_file(file, 'phrase_to_file.txt').

phrase_to_file(Grammar, File) :-
    phrase_string(Grammar, String),        % pure
    setup_call_cleanup(
        open(File, write, Stream),
        write(Stream, String),          % minimal side effect here
        close(Stream)
    ).
