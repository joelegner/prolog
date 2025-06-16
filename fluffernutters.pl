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
show(1, piano_bar, date(2025, 7, 6), time(22, 30), duration_hours(3)). 
show(2, pool, date(2025, 7, 8), time(14, 0), duration_hours(1)). 
show(3, piano_bar, date(2025, 7, 9), time(22, 30, duration_hours(3)). 

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
