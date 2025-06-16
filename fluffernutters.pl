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

% SATURDAY, JULY 5 EVENTS
event(1, nude_pool, 'Pool tunes with DJ Gene Gene', date(2025, 7, 5), time(14, 0), duration_hours(3)).
event(2, disco, 'Welcome to Hedo Kick-off Party with DJ Gene Gene', date(2025, 7, 5), time(23, 0), duration_hours(2)).

% SUNDAY, JULY 6 EVENTS
event(3, piano_bar, 'Fluffernutter orientation for newbies and vets!', date(2025, 7, 6), time(10, 30), duration_hours(0.75)).
event(4, piano_bar, 'Resort Tour with Fluff Ambassadors', date(2025, 7, 6), time(11, 15), duration_hours(1)).
event(5, disco, 'Cock \'n Tail Party--Purple Tickets, Drawing Card, School Supplies, Donations', date(2025, 7, 6), time(16, 00), duration_hours(2)).

% MONDAY, JULY 7 EVENTS
event(6, nude_pool, 'Classic Pool Fun featuring DJ Gene Gene', date(2025, 7, 7), time(14,00), duration_hours(3)).
event(7, piano_bar, 'Bourbon/Whiskey Tasting', date(2025, 7, 7), time(16,00), duration_hours(1.5)).
event(8, prude_beach, 'Interactive Vibrating Dinner (remotes exchanged at 7:00 pm)', date(2025, 7, 7), time(18,30), duration_hours(0.5)).
event(9, prude_beach, 'Dinner on the Beach (Weather permitting)', date(2025, 7, 7), time(19,00), duration_hours(3)).
event(10, pier, 'Chinese lantern send off with music by John & Chuck', date(2025, 7, 7), time(22,30), duration_hours(0.5)).
event(11, nude_pool, 'Moonlight Naked Pool Party', date(2025, 7, 7), time(23,30), duration_hours(2)).

% days_before(+StartDate, +Days, -EndDate)
days_before(date(Y, M, D), Days, date(Y2, M2, D2)) :-
    FullStart = date(Y, M, D, 0, 0, 0, 0, -, -),
    date_time_stamp(FullStart, StartStamp),
    DeltaSeconds is Days * 86400,
    TargetStamp is StartStamp - DeltaSeconds,
    stamp_date_time(TargetStamp, date(Y2, M2, D2, _, _, _, _, _, _), 'UTC').

% format_date_mdyyyy(+Date, -Formatted)
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

% days_diff(+Date1, +Date2, -Diff)
days_diff(date(Y1, M1, D1), date(Y2, M2, D2), Diff) :-
    date_time_stamp(date(Y1, M1, D1, 0,0,0,0,-,-), Stamp1),
    date_time_stamp(date(Y2, M2, D2, 0,0,0,0,-,-), Stamp2),
    SecondsDiff is Stamp2 - Stamp1,
    Diff is round(SecondsDiff / 86400).

% print_human_task_list/0
print_human_task_list :-
    get_time(NowStamp),
    stamp_date_time(NowStamp, date(Y, M, D, _, _, _, _, _, _), local),
    Today = date(Y, M, D),
    start_date(fluffernutters, EventStart),
    forall(
        task(DaysBefore, TaskName),
        (
            days_before(EventStart, DaysBefore, DueDate),
            days_diff(Today, DueDate, DaysUntil),
            DaysUntil >= 0,
            format_date_mdyyyy(DueDate, DateStr),
            format('In ~w days on ~w ~w (~w days before Jamaica)~n', [DaysUntil, DateStr, TaskName, DaysBefore])
        )
    ).

% dt_ical(+Date, +Time, -DTStr)
dt_ical(date(Y,M,D), time(H,Min), DTStr) :-
    format(atom(DTStr), '~d~|~`0t~d~2+~|~`0t~d~2+T~|~`0t~d~2+~|~`0t~d~2+00', [Y, M, D, H, Min]).

% add_duration(+Date, +Time, +duration_hours(Hours), -NewDate, -NewTime)
add_duration(date(Y, M, D), time(H, Min), duration_hours(Duration), NewDate, time(NewH, NewMin)) :-
    DurationMinutesF is Duration * 60,
    DurationMinutes is round(DurationMinutesF),
    TotalMinutes is H * 60 + Min + DurationMinutes,
    NewHRaw is TotalMinutes // 60,
    NewMin is TotalMinutes mod 60,
    ( NewHRaw < 24 ->
        NewDate = date(Y, M, D),
        NewH = NewHRaw
    ; ExtraDays is NewHRaw // 24,
      NewH is NewHRaw mod 24,
      days_before(date(Y, M, D), -ExtraDays, NewDate)
    ).

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
        show(ID, Venue, Date, StartTime, Duration),
        (
            Name = 'The Wonder Twins',
            add_duration(Date, StartTime, Duration, EndDate, EndTime),
            dt_ical(Date, StartTime, DTSTART),
            dt_ical(EndDate, EndTime, DTEND),
            generate_uid('show', ID, Date, UID),
            atom_upper(Venue, VenueCaps),
            format(atom(Title), '~w: ~w', [VenueCaps, Name]),
            print_event(Title, Venue, DTSTART, DTEND, UID)
        )
    ),

    % Other events
    forall(
        event(ID, Venue, Desc, Date, StartTime, Duration),
        (
            add_duration(Date, StartTime, Duration, EndDate, EndTime),
            dt_ical(Date, StartTime, DTSTART),
            dt_ical(EndDate, EndTime, DTEND),
            generate_uid('event', ID, Date, UID),
            atom_upper(Venue, VenueCaps),
            format(atom(Title), '~w: ~w', [VenueCaps, Desc]),
            print_event(Title, Venue, DTSTART, DTEND, UID)
        )
    ),

    writeln('END:VCALENDAR').

% print_event(+Summary, +Venue, +DTSTART, +DTEND, +UID)
print_event(Summary, Venue, DTSTART, DTEND, UID) :-
    writeln('BEGIN:VEVENT'),
    format('UID:~w@fluffernutters.com~n', [UID]),
    format('SUMMARY:~w~n', [Summary]),
    format('DESCRIPTION:Venue: ~w~n', [Venue]),
    format('DTSTART;TZID=America/New_York:~w~n', [DTSTART]),
    format('DTEND;TZID=America/New_York:~w~n', [DTEND]),
    writeln('END:VEVENT').
