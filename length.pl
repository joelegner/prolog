% length.pl
% ChatGPT wrote the first draft of this at my direction.
% Demonstrates handling lengths and writing them.

write_length(feet(N), Digits) :-
    format('~*f ft~n', [Digits, N]).

% Default case: use 3 digits.
write_length(feet(N)) :- 
    write_length(feet(N), 3).

run :-
    write_length(feet(6), 0),
    write_length(feet(6), 1),
    write_length(feet(6), 2),
    write_length(feet(6)),
    write_length(feet(6), 4).