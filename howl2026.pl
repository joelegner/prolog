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

% March 2026 - Bahamas: Great Stirrup Cay & Nassau
itinerary(5,
    'Bahamas: Great Stirrup Cay & Nassau',
    getaway,
    miami,
    [nassau, great_stirrup_cay],
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

% June 2026 - Breakaway: Halifax & Bermuda
itinerary(13,
    'Halifax & Bermuda',
    escape,
    boston,
    [boston, bermuda, halifax],
    boston
).

% August 2026 - Escape: Caribbean: Dominican Republic & St. Thomas
itinerary(14,
    'Caribbean: Dominican Republic & St. Thomas',
    escape,
    new_york,
    [new_york, puerto_rico, st_maarten, st_thomas, tortola, puerto_plata],
    new_york
).

% August 2026 - Escape: Caribbean: Dominican Republic & St. Thomas
itinerary(15,
    'Caribbean: Harvest Caye, Cozumel, Roatan',
    escape,
    miami,
    [miami, cozumel, roatan, harvest_caye, costa_maya],
    miami
).

% August 2026 - Canada & New England: Boston & Halifax
itinerary(16,
    'Canada & New England: Boston & Halifax',
    escape,
    new_york,
    [new_york, boston, portland_me, saint_john, halifax],
    new_york
).

% August 2026
itinerary(17,
    'Halifax & Bermuda',
    breakaway,
    boston,
    [boston, halifax, bermuda],
    boston
).

% August 2026
itinerary(18,
    'Canada & New England: Bar Harbor & Halifax',
    breakaway,
    boston,
    [boston, portland, bar_harbor, saint_john, halifax],
    boston
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
cruise(3, [2026, 1, 19], [2026, 1, 29]).

% Itinerary 4 - Getaway Bahamas Runs 1 and 3
cruise(4, [2026, 1, 15], [2026, 1, 19]).
cruise(4, [2026, 1, 29], [2026, 2, 2]).

% Itinerary 5 - Getaway Miami to Bahamas 4-day March 2026 (example)
cruise(5, [2026, 3, 2], [2026, 3, 6]).
cruise(5, [2026, 3, 6], [2026, 3, 9]).
cruise(5, [2026, 3, 9], [2026, 3, 13]).
cruise(5, [2026, 3, 13], [2026, 3, 16]).
cruise(5, [2026, 3, 16], [2026, 3, 20]).
cruise(5, [2026, 3, 20], [2026, 3, 23]).
cruise(5, [2026, 3, 23], [2026, 3, 27]).
cruise(5, [2026, 3, 27], [2026, 3, 30]).
cruise(5, [2026, 3, 30], [2026, 4, 3]).

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
cruise(9, [2026, 5, 24], [2026, 5, 31]).
cruise(9, [2026, 5, 31], [2026, 6, 7]).

% Itinerary 11 - Escape Spain & Azores September-October 2026
cruise(11, [2026, 9, 26], [2026, 10, 12]).

% Itinerary 12 - Escape Transatlantic October-November 2026
cruise(12, [2026, 10, 28], [2026, 11, 13]).

% Itinerary 13 -- Breakaway: Halifax & Bermuda-June 2026
cruise(13, [2026, 6, 7], [2026, 6, 14]).
cruise(13, [2026, 6, 14], [2026, 6, 21]).
cruise(13, [2026, 6, 21], [2026, 6, 28]).
cruise(13, [2026, 6, 28], [2026, 7, 5]).

% Itinerary 14 -- Escape: Caribbean
cruise(14, [2026, 8, 19], [2026, 8, 29]).

% Itinerary 15 -- Caribbean: Harvest Caye, Cozumel, Roatan
cruise(15, [2026, 8, 2], [2026, 8, 9]).
cruise(15, [2026, 8, 9], [2026, 8, 16]).

% Itinerary 16
cruise(16, [2026, 8, 29], [2026, 9, 5]).
cruise(16, [2026, 9, 5], [2026, 9, 12]).
cruise(16, [2026, 9, 12], [2026, 9, 19]).
cruise(16, [2026, 9, 19], [2026, 9, 26]).

% Itinerary 17
cruise(17, [2026, 8, 2], [2026, 8, 9]).
cruise(17, [2026, 8, 9], [2026, 8, 16]).
cruise(17, [2026, 8, 16], [2026, 8, 23]).

% Itinerary 18
cruise(18, [2026, 8, 23], [2026, 8, 30]).
cruise(18, [2026, 8, 30], [2026, 9, 6]).

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
         date_to_ics(Start, DTSTART, start),
         date_to_ics(End, DTEND, end),
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

date_to_ics([Y, M, D], DateStr, start) :-
    format(atom(DateStr), '~d~|~`0t~d~2+~|~`0t~d~2+', [Y, M, D]).

date_to_ics([Y, M, D], DateStr, end) :-
    D1 is D+1,
    format(atom(DateStr), '~d~|~`0t~d~2+~|~`0t~d~2+', [Y, M, D1]).

generate_uid(ItinID, [Y, M, D], UID) :-
    format(atom(UID), 'itin~w-~w~|~`0t~w~2+~|~`0t~w~2+', [ItinID, Y, M, D]).
