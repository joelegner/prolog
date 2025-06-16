% fluffernutters.pl
:- use_module(library(date)).

event(fluffernutters).
start_date(fluffernutters, date(2025, 7, 4)).
end_date(fluffernutters, date(2025, 7, 12)).

% task(DaysBeforeStartDate, TaskName)
task(14, 'Test coverage').
task( 5, 'Start packing').
task( 5, 'Manscape').
task( 3, 'Trim beard').
task( 2, 'Pedicure').
task( 1, 'Apply coverage').
task( 0, 'Shave legs').
task( 0, 'Fly to Jamaica!').

% Events during the week

% ShowID, Venue, Date, StartTime, Duration
show(1, piano_bar, date(2025, 7, 6), time(22, 30), duration_hours(3)). 
show(2, pool, date(2025, 7, 8), time(14, 0), duration_hours(1)). 
show(3, piano_bar, date(2025, 7, 9), time(22, 30), duration_hours(3)). 

% EventID, Venue, Description, Date, StartTime, Duration
event( 1, nude_pool, 'DJ Gene Gene', date(2025, 7, 5), time(14, 0), duration_hours(3)).
event( 2, disco, 'DJ Gene Gene', date(2025, 7, 5), time(23, 0), duration_hours(2)).

% days_before(+StartDate, +Days, -EndDate)
days_before(date(Y, M, D), Days, date(Y2, M2, D2)) :-
    FullStart = date(Y, M, D, 0, 0, 0, 0, -, -),
    date_time_stamp(FullStart, StartStamp),
    DeltaSeconds is Days * 86400,
    TargetStamp is StartStamp - DeltaSeconds,
    stamp_date_time(TargetStamp, date(Y2, M2, D2, _, _, _, _, _, _), 'UTC').

% format_date_mdyyyy(+Date, -Formatted)
% Formats date(Y, M, D) as "MM/DD/YYYY" with zero-padded month and day
format_date_mdyyyy(date(Y, M, D), Formatted) :-
    format(string(Formatted), '~|~`0t~d~2+/~|~`0t~d~2+/~d', [M, D, Y]).

% print_task_list/0
print_task_list :-
    start_date(fluffernutters, EventStart),
    forall(
        task(DaysBefore, TaskName),
        (
            days_before(EventStart, DaysBefore, DueDate),
            format_date_mdyyyy(DueDate, DateStr),
            format('~w: ~w~n', [DateStr, TaskName])
        )
    ).

% Helper predicate to compute days difference between two dates (Date2 - Date1)
days_diff(date(Y1, M1, D1), date(Y2, M2, D2), Diff) :-
    date_time_stamp(date(Y1, M1, D1, 0,0,0,0,-,-), Stamp1),
    date_time_stamp(date(Y2, M2, D2, 0,0,0,0,-,-), Stamp2),
    SecondsDiff is Stamp2 - Stamp1,
    Diff is round(SecondsDiff / 86400).

% print_human_task_list/0
print_human_task_list :-
    % Get today's date (local time)
    get_time(NowStamp),
    stamp_date_time(NowStamp, date(Year, Month, Day, _, _, _, _, _, _), local),
    Today = date(Year, Month, Day),

    % Get event start date
    start_date(fluffernutters, EventStart),

    forall(
        task(DaysBefore, TaskName),
        (
            days_before(EventStart, DaysBefore, DueDate),
            days_diff(Today, DueDate, DaysUntil),
            DaysUntil >= 0,  % only print tasks due today or later
            format_date_mdyyyy(DueDate, DateStr),
            format('In ~w days on ~w ~w (~w days before Jamaica)~n', [DaysUntil, DateStr, TaskName, DaysBefore])
        )
    ).

% Calendar stuff

/*
BEGIN:VEVENT
UID:itin18-20260830@ncl.com
SUMMARY:BREAKAWAY: Canada & New England: Bar Harbor & Halifax
DESCRIPTION:Venue: breakaway
DTSTART;VALUE=DATE:20260830
DTEND;VALUE=DATE:20260907
END:VEVENT
*/

% date_to_ics(+date(Y,M,D), -String)
date_to_ics(date(Y, M, D), ICS) :-
    format(atom(ICS), '~d~|~`0t~d~2+~|~`0t~d~2+', [Y, M, D]).

% date_add_one_day(+Date, -NextDate)
date_add_one_day(Date, NextDate) :-
    days_before(Date, -1, NextDate).

% generate_uid(+Prefix, +ID, +Date, -UID)
generate_uid(Prefix, ID, date(Y, M, D), UID) :-
    format(atom(UID), '~w~w-~d~|~`0t~d~2+~|~`0t~d~2+', [Prefix, ID, Y, M, D]).

% atom_upper(+Atom, -UpperAtom)
atom_upper(Atom, Upper) :-
    atom_string(Atom, Str),
    string_upper(Str, UpperStr),
    atom_string(Upper, UpperStr).

% show_calendar/0
show_calendar :-
    writeln('BEGIN:VCALENDAR'),
    writeln('VERSION:2.0'),
    writeln('PRODID:-//Fluffernutters//Calendar Export//EN'),

    % Shows
    forall(
        show(ID, Venue, Date, _, _),
        (
            Name = 'The Wonder Twins',
            date_to_ics(Date, DTSTART),
            date_add_one_day(Date, Date2),
            date_to_ics(Date2, DTEND),
            generate_uid('show', ID, Date, UID),
            atom_upper(Venue, VenueCaps),
            format(atom(Title), '~w: ~w', [VenueCaps, Name]),
            print_event(Title, Venue, DTSTART, DTEND, UID)
        )
    ),

    % Other events
    forall(
        event(ID, Venue, Desc, Date, _, _),
        (
            date_to_ics(Date, DTSTART),
            date_add_one_day(Date, Date2),
            date_to_ics(Date2, DTEND),
            generate_uid('event', ID, Date, UID),
            atom_upper(Venue, VenueCaps),
            format(atom(Title), '~w: ~w', [VenueCaps, Desc]),
            print_event(Title, Venue, DTSTART, DTEND, UID)
        )
    ),

    % Tasks
    start_date(fluffernutters, EventStart),
    forall(
        task(DaysBefore, Task),
        (
            days_before(EventStart, DaysBefore, Date),
            date_to_ics(Date, DTSTART),
            date_add_one_day(Date, Date2),
            date_to_ics(Date2, DTEND),
            generate_uid('task', DaysBefore, Date, UID),
            print_event(Task, 'Task', DTSTART, DTEND, UID)
        )
    ),

    writeln('END:VCALENDAR').

% print_event(+Summary, +Venue, +DTSTART, +DTEND, +UID)
print_event(Summary, Venue, DTSTART, DTEND, UID) :-
    writeln('BEGIN:VEVENT'),
    format('UID:~w@fluffernutters.com~n', [UID]),
    format('SUMMARY:~w~n', [Summary]),
    format('DESCRIPTION:Venue: ~w~n', [Venue]),
    format('DTSTART;VALUE=DATE:~w~n', [DTSTART]),
    format('DTEND;VALUE=DATE:~w~n', [DTEND]),
    writeln('END:VEVENT').
