%% trip_planning.pl

:- op(100, xfx, at).
:- op(50, xfx, :).

% ferry schedules (timetable/1 facts grouped together)
timetable([riva at 8:00, limone at 8:35, malcesine at 8:55]).
timetable([riva at 9:10, torbole at 9:25, limone at 9:55, malcesine at 10:15]).
timetable([riva at 9:45, torbole at 10:00, limone at 10:30, malcesine at 10:50]).
timetable([riva at 11:45, torbole at 12:00, limone at 12:30, malcesine at 12:50]).
timetable([riva at 13:10, limone at 13:32, malcesine at 13:45]).
timetable([riva at 14:05, limone at 14:40, malcesine at 15:00]).
timetable([riva at 15:00, limone at 15:36, malcesine at 15:57, campione at 16:13]).
timetable([riva at 16:20, torbole at 16:35, limone at 17:05, malcesine at 17:25]).
timetable([riva at 18:05, torbole at 18:20, limone at 18:50, malcesine at 19:10]).
timetable([malcesine at 9:00, limone at 9:20, torbole at 9:50, riva at 10:05]).
timetable([malcesine at 10:25, limone at 10:50, torbole at 11:20, riva at 11:35]).
timetable([malcesine at 11:25, limone at 11:45, riva at 12:20]).
timetable([campione at 12:55, malcesine at 13:12, limone at 13:34, riva at 14:10]).
timetable([malcesine at 13:45, limone at 13:59, riva at 14:20]).
timetable([malcesine at 15:05, limone at 15:25, riva at 16:00]).
timetable([malcesine at 16:30, limone at 16:50, torbole at 17:20, riva at 17:35]).
timetable([malcesine at 18:15, limone at 18:35, torbole at 19:05, riva at 19:20]).
timetable([malcesine at 19:15, limone at 19:35, torbole at 20:05, riva at 20:20]).

% main planning rule
schedule(Start, Destination, [depart(Start), arrive(Next) | Rest]) :-
    sail(Start, Next),
    rest_schedule(Next, Destination, Rest).

% already at destination
rest_schedule(Place, Place, []).

% continue onboard to next destination
rest_schedule(CurrentPlace, Destination, [arrive(Next) | Rest]) :-
    sail(CurrentPlace, Next),
    rest_schedule(Next, Destination, Rest).

% wait for next departure from Place at a later time
rest_schedule(Place at Time1, Destination, [stay(Place, Time1, Time2) | Rest]) :-
    sail(Place at Time2, _),
    before(Time1, Time2),
    schedule(Place at Time2, Destination, Rest).

% sail without time — location only
sail(Place1, Place2) :-
    timetable(List),
    append(_, [Place1, Place2 | _], List).

% sail with time
sail(Place1 at Time1, Place2 at Time2) :-
    timetable(List),
    append(_, [Place1 at Time1, Place2 at Time2 | _], List).

% time difference in minutes
time_diff(H1:Min1, H2:Min2, Diff) :-
    Diff is 60 * (H2 - H1) + Min2 - Min1.

% true if Time1 is before Time2
before(H1:M1, H2:M2) :-
    H1 < H2 ;
    (H1 =:= H2, M1 < M2).

before(Time1, Time2) :- 
    time_diff(Time1, Time2, Diff), 
    Diff > 0.

list([]).

list([_|L]) :- list(L). 

/*
Sample usage:

?- schedule(riva at 9:10, malcesine at FinalTime, Schedule).
FinalTime = 10:15,
Schedule = [depart(riva at 9:10), arrive(torbole at 9:25), arrive(limone at 9:55), arrive(malcesine at 10:15)] 
*/
