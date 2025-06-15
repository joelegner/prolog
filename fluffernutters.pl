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
