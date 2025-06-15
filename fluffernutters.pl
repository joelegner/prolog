% fluffernutters.pl
:- use_module(library(date)).

event(fluffernutters).
start_date(fluffernutters, date(2025, 7, 4)).
end_date(fluffernutters, date(2025, 7, 12)).

task('Trim beard', 3).
task('Manscape', 5).
task('Pedicure', 2).
task('Shave legs', 0).
task('Test coverage', 14).
task('Apply coverage', 1).
task('Start packing', 5).

% days_before(+StartDate, +Days, -EndDate)
% EndDate is Days days before StartDate (real calendar-aware)
days_before(date(Y, M, D), Days, date(Y2, M2, D2)) :-
    % Convert to full date/9 with dummy time and timezone
    FullStart = date(Y, M, D, 0, 0, 0, 0, -, -),
    date_time_stamp(FullStart, StartStamp),
    DeltaSeconds is Days * 86400,
    TargetStamp is StartStamp - DeltaSeconds,
    stamp_date_time(TargetStamp, date(Y2, M2, D2, _, _, _, _, _, _), 'UTC').

% format_date_mdyyyy(+Date, -Formatted)
% Formats a date(Y, M, D) as "M/D/YYYY"
format_date_mdyyyy(date(Y, M, D), Formatted) :-
    format(string(Formatted), '~w/~w/~w', [M, D, Y]).


% print_task_list/0
% Prints all tasks with their due dates
print_task_list :-
    start_date(fluffernutters, EventStart),
    forall(
        task(TaskName, DaysBefore),
        (
            days_before(EventStart, DaysBefore, DueDate),
            format_date_mdyyyy(DueDate, DateStr),
            format('~w: ~w~n', [TaskName, DateStr])
        )
    ).
