%% phrase_to_file_example.pl
:- use_module(phrase_to_file).
:- use_module(book).

file --> 
    "Hello, World!".

main :-
    phrase_to_file(file, 'phrase_to_file.txt').

book :-
    phrase_to_file(book, 'phrase_to_file.tex').