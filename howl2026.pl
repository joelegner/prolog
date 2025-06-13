ship(escape).
ship(epic).
ship(getaway).
ship(breakaway).

% itinerary(ItineraryID, NCLName, Ship, StartPort, [Ports], EndPort).
itinerary(1, 
    'Carribbean: Harvest Caye, Cozumel & Roatan',
    escape,
    new_orleans,
    [cozumel, harvest_caye, roatan, coasta_maya],
    new_orleans
).

itinerary(2, 
    'Carribbean: Great Stirrup Cay & Nassau',
    getaway,
    miami,
    [nassau, great_stirrup_cay],
    miami
).

% cruise(ItineraryID, StartDate, EndDate)
cruise(1, [2026, 1,  4], [2026, 1, 11]).
cruise(1, [2026, 1, 11], [2026, 1, 18]).
cruise(1, [2026, 1, 18], [2026, 1, 25]).
cruise(1, [2026, 1, 25], [2026, 2,  1]).
cruise(1, [2026, 2, 15], [2026, 2, 22]).
cruise(1, [2026, 3,  8], [2026, 2, 22]).
cruise(1, [2026, 3, 15], [2026, 3, 22]).
cruise(1, [2026, 3, 22], [2026, 3, 29]).
cruise(1, [2026, 3, 29], [2026, 4,  5]).
cruise(1, [2026, 4,  5], [2026, 4, 12]).
cruise(1, [2026, 4, 12], [2026, 4, 19]).
cruise(1, [2026, 4, 19], [2026, 4, 26]).
cruise(1, [2026, 4, 26], [2026, 5,  3]).
cruise(1, [2026, 5,  3], [2026, 5, 10]).
cruise(1, [2026, 5, 10], [2026, 5, 17]).

cruise(2, [2026, 1, 15], [2026, 1, 19]).
cruise(2, [2026, 1, 29], [2026, 2, 2]).
cruise(2, [2026, 2, 12], [2026, 2, 16]).
cruise(2, [2026, 2, 16], [2026, 2, 20]).
cruise(2, [2026, 3, 2], [2026, 3, 6]).
cruise(2, [2026, 3, 9], [2026, 3, 13]).
cruise(2, [2026, 3, 16], [2026, 3, 20]).
cruise(2, [2026, 3, 23], [2026, 3, 27]).
cruise(2, [2026, 3, 30], [2026, 4, 3]).

gantt :-
    writeln('@startgantt'),
    writeln('printscale weekly'),
    writeln('Project starts 2025-12-01'),
    forall(
        (cruise(ItinID, Start, End),
         itinerary(ItinID, Name, _, _, _, _),
         format_time_string(Start, StartStr),
         format_time_string(End, EndStr)),
        format('[~w] starts ~w and ends ~w~n', [Name, StartStr, EndStr])
    ),
    writeln('@endgantt').

format_time_string([Y,M,D], Formatted) :-
    format(atom(Formatted), '~d-~|~`0t~d~2+-~|~`0t~d~2+', [Y, M, D]).
