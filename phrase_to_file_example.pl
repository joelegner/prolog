%% phrase_to_file_example.pl
:- use_module(phrase_to_file).

file --> 
    "Hello, World!!".

main :-
    phrase_to_file(file, 'phrase_to_file.txt').
