practiced(Player, NewSongs, OldSongs) :-
    tries(Player, NewSongs),
    practices(Player, OldSongs).

tries(_, []).
tries(Player, [Song | Songs]) :-
    format("~w tries ~w.~n", [Player, Song]),
    tries(Player, Songs).

practices(_, []).
practices(Player, [Song | Songs]) :-
    format("~w practices ~w.~n", [Player, Song]),
    practices(Player, Songs).

main :-
    writeln('World simulator 2025-04-20 Blaze It!'),
    practiced(joe, [song1, song2, song3], [song4, song5, song6]).

