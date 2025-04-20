practiced(Player, NewSongs, OldSongs) :-
    tried(Player, NewSongs),
    practiced(Player, OldSongs).

tried(_, []).
tried(Player, [Song | Songs]) :-
    format("~w tried ~w.~n", [Player, Song]),
    tried(Player, Songs).

practiced(_, []).
practiced(Player, [Song | Songs]) :-
    format("~w practiced ~w.~n", [Player, Song]),
    practiced(Player, Songs).

main :-
    writeln('World simulator 2025-04-20 Blaze It!'),
    practiced(joe, [song1, song2, song3], [song4, song5, song6]).
