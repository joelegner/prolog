% Ships
ship(escape).
ship(epic).
ship(getaway).
ship(breakaway).  % included for completeness, no runs given

% Itineraries (ID, Name, Ship, StartPort, PortsList, EndPort)

% December 2025 - Escape - Caribbean: Harvest Caye, Cozumel, Roatan
itinerary(1, 
    'Caribbean: Harvest Caye, Cozumel & Roatan',
    escape,
    new_orleans,
    [cozumel, harvest_caye, roatan],
    new_orleans
).

% January 2026 - Epic - Caribbean: Barbados, Antigua, St. Lucia
itinerary(2,
    'Caribbean: Barbados, Antigua & St. Lucia',
    epic,
    st_maarten,
    [barbados, antigua, st_lucia],
    st_maarten
).

% January 2026 - Getaway - Caribbean: Dominican Republic & Antigua
itinerary(3,
    'Caribbean: Dominican Republic & Antigua',
    getaway,
    miami,
    [dominican_republic, antigua],
    miami
).

% January 2026 - Getaway - Bahamas: Great Stirrup Cay & Nassau
itinerary(4,
    'Bahamas: Great Stirrup Cay & Nassau',
    getaway,
    miami,
    [great_stirrup_cay, nassau],
    miami
).

% March 2026 - Getaway - Bounced Miami to Bahamas (4-day)
itinerary(5,
    'Bounced Miami to Bahamas',
    getaway,
    miami,
    [nassau],
    miami
).

% March-April 2026 - Epic - Caribbean: Barbados, Antigua, St. Lucia
itinerary(6,
    'Caribbean: Barbados, Antigua & St. Lucia',
    epic,
    st_maarten,
    [barbados, antigua, st_lucia],
    st_maarten
).

% April 2026 - Epic - Caribbean: Curacao, Aruba, Bonaire
itinerary(7,
    'Caribbean: Curacao, Aruba & Bonaire',
    epic,
    curacao,
    [aruba, bonaire],
    curacao
).

% April-May 2026 - Epic - Transatlantic: San Juan PR to Spain & Gibraltar
itinerary(8,
    'Transatlantic: San Juan PR to Spain & Gibraltar',
    epic,
    san_juan_pr,
    [spain, gibraltar],
    spain
).

% May-June 2026 - Epic - Mediterranean: Nice, Florence, Amalfi Coast (Salerno)
itinerary(9,
    'Mediterranean: Nice, Florence & Amalfi Coast (Salerno)',
    epic,
    nice,
    [florence, marseille, rome, salerno],
    nice
).

% August-September 2026 - Escape - Canada & New England
itinerary(10,
    'Canada & New England',
    escape,
    new_york,
    [boston, portland_me, halifax],
    new_york
).

% September 2026 - Escape - Spain & Azores
itinerary(11,
    'Spain & Azores',
    escape,
    new_york,
    [azores],
    new_york
).

% October 2026 - Escape - Transatlantic
itinerary(12,
    'Transatlantic',
    escape,
    new_york,
    [london, lisbon],  % placeholders for Spain & Gibraltar etc.
    london
).

% Other Epic Mediterranean cruises (July, August, September, October 2026)
% These can reuse itinerary 9 or be split further if needed.

% Cruise runs - for example:

% Itinerary 1 - Escape - December 14, 2025 and January 4, 2026, two 7-day cruises
cruise(1, [2025, 12, 14], [2026, 1, 4]).

% Itinerary 2 - Epic Caribbean Runs Jan 2026
cruise(2, [2026, 1, 4], [2026, 1, 11]).
cruise(2, [2026, 1, 11], [2026, 1, 18]).
cruise(2, [2026, 1, 18], [2026, 1, 25]).
cruise(2, [2026, 1, 25], [2026, 2, 1]).

% Itinerary 3 - Getaway Caribbean Run 2
cruise(3, [2026, 1, 19], [2026, 1, 26]).

% Itinerary 4 - Getaway Bahamas Runs 1 and 3
cruise(4, [2026, 1, 15], [2026, 1, 19]).
cruise(4, [2026, 1, 29], [2026, 2, 2]).

% Itinerary 5 - Getaway Miami to Bahamas 4-day March 2026 (example)
cruise(5, [2026, 3, 1], [2026, 3, 5]).

% Itinerary 6 - Epic Caribbean March-April 2026 Runs 1-5
cruise(6, [2026, 3, 1], [2026, 3, 8]).
cruise(6, [2026, 3, 8], [2026, 3, 15]).
cruise(6, [2026, 3, 15], [2026, 3, 22]).
cruise(6, [2026, 3, 22], [2026, 3, 29]).
cruise(6, [2026, 3, 29], [2026, 4, 5]).

% Itinerary 7 - Epic Caribbean April 2026 Curacao etc.
cruise(7, [2026, 4, 12], [2026, 4, 19]).

% Itinerary 8 - Epic Transatlantic April-May 2026
cruise(8, [2026, 4, 19], [2026, 5, 3]).

% Itinerary 9 - Epic Mediterranean May-June 2026 Runs 1-3
cruise(9, [2026, 5, 3], [2026, 5, 10]).
cruise(9, [2026, 5, 10], [2026, 5, 17]).
cruise(9, [2026, 5, 17], [2026, 5, 24]).
cruise(9, [2026, 5, 31], [2026, 6, 7]).

% Itinerary 10 - Escape Canada & New England August-September 2026
cruise(10, [2026, 8, 29], [2026, 9, 5]).
cruise(10, [2026, 9, 5], [2026, 9, 12]).
cruise(10, [2026, 9, 12], [2026, 9, 19]).
cruise(10, [2026, 9, 19], [2026, 9, 26]).

% Itinerary 11 - Escape Spain & Azores September-October 2026
cruise(11, [2026, 9, 26], [2026, 10, 12]).

% Itinerary 12 - Escape Transatlantic October-November 2026
cruise(12, [2026, 10, 28], [2026, 11, 13]).


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
