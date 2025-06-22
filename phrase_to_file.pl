%% phrase_to_file.pl

:- use_module(phrase_string).

file --> 
    "Hello, World".

main :-
    phrase_string(file, String),        % pure
    setup_call_cleanup(
        open('phrase_to_file.txt', write, Stream),
        write(Stream, String),          % minimal side effect here
        close(Stream)
    ).
