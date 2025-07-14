%% pasteboard.pl

:- module(pasteboard, [pbcopy/1, pbpaste/1]).

%% pbcopy(+Text)
%  Copy a string or atom to the macOS pasteboard.
pbcopy(Text) :-
    atom(Text), !,
    pbcopy_atom(Text).
pbcopy(Text) :-
    string(Text),
    atom_string(Atom, Text),
    pbcopy_atom(Atom).

pbcopy_atom(Atom) :-
    open(pipe('pbcopy'), write, Stream),
    write(Stream, Atom),
    close(Stream).

%% pbpaste(-Text)
%  Retrieve the contents of the macOS pasteboard as a string.
pbpaste(Text) :-
    open(pipe('pbpaste'), read, Stream),
    read_string(Stream, _, Text),
    close(Stream).

:- begin_tests(pasteboard).
:- use_module(pasteboard).

test(copy_and_paste_string) :-
    pbcopy("Hello, clipboard!"),
    pbpaste(Text),
    assertion(Text == "Hello, clipboard!").

test(copy_and_paste_atom) :-
    pbcopy(hello_clipboard),
    pbpaste(Text),
    assertion(Text == "hello_clipboard").

test(copy_and_paste_empty_string) :-
    pbcopy(""),
    pbpaste(Text),
    assertion(Text == "").

test(copy_and_paste_multiline) :-
    MultiLine = "Line 1\nLine 2\nLine 3",
    pbcopy(MultiLine),
    pbpaste(Text),
    assertion(Text == MultiLine).

:- end_tests(pasteboard).
