% Notes on a piano keyboard
:- use_module(piano).

print_keys :-
    key(N, Name),
    format('~w: ~w~n', [N, Name]),
    fail.
print_keys.