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

calendar :-
    writeln('BEGIN:VCALENDAR'),
    writeln('VERSION:2.0'),
    writeln('PRODID:-//NCL Cruises//Calendar Export//EN'),
    forall(
        (cruise(ItinID, Start, End),
         itinerary(ItinID, Name, Ship, _, _, _),
         date_to_ics(Start, DTSTART),
         date_to_ics(End, DTEND),
         generate_uid(ItinID, Start, UID)),
        print_event(Name, Ship, DTSTART, DTEND, UID)
    ),
    writeln('END:VCALENDAR').

print_event(Name, Ship, DTSTART, DTEND, UID) :-
    atom_upper(Ship, ShipCaps),
    format(atom(Title), '~w: ~w', [ShipCaps, Name]),
    writeln('BEGIN:VEVENT'),
    format('UID:~w@ncl.com~n', [UID]),
    format('SUMMARY:~w~n', [Title]),
    format('DESCRIPTION:Ship: ~w~n', [Ship]),
    format('DTSTART;VALUE=DATE:~w~n', [DTSTART]),
    format('DTEND;VALUE=DATE:~w~n', [DTEND]),
    writeln('END:VEVENT').

atom_upper(Atom, Upper) :-
    atom_string(Atom, Str),
    string_upper(Str, UpperStr),
    atom_string(Upper, UpperStr).

char_upper(Char, Upper) :-
    char_type(Char, to_upper(Upper)).

date_to_ics([Y, M, D], DateStr) :-
    format(atom(DateStr), '~d~|~`0t~d~2+~|~`0t~d~2+', [Y, M, D]).

generate_uid(ItinID, [Y, M, D], UID) :-
    format(atom(UID), 'itin~w-~w~|~`0t~w~2+~|~`0t~w~2+', [ItinID, Y, M, D]).
