% length.pl
% ChatGPT wrote the first draft of this at my direction.
% Demonstrates handling lengths and writing them.

write_length(feet(N), Digits) :-
    format('~*f ft~n', [Digits, N]).

run :-
    write_length(feet(6), 3).