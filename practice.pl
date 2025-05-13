% practice.pl
% This one demonstrates list processing.
practiced(Player, NewSongs, OldSongs) :-
    tried(Player, NewSongs),
    practiced(Player, OldSongs).

% Notice the list-processing paradigm here:
% We handle the base case with an empty list, then recursively process the head of the list (Song),
% performing an action (printing) and recursing on the tail (Songs).
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
