%% time.pl

/*
What I want to do is create a time predicate that gets the time from the command line and parses it.
*/
%% time.pl
:- module(time, [datetimestamp//1]).

:- use_module(library(dcg/basics)).

datetimestamp(date(DayOfWeek, Month, Day, Time, TZ, Year)) -->
    day_of_week(DayOfWeek), " ",
    month(Month), " ",
    integer(Day), " ",
    time(Time), " ",
    timezone(TZ), " ",
    integer(Year).

day_of_week(mon) --> "Mon".
day_of_week(tue) --> "Tue".
day_of_week(wed) --> "Wed".
day_of_week(thu) --> "Thu".
day_of_week(fri) --> "Fri".
day_of_week(sat) --> "Sat".
day_of_week(sun) --> "Sun".

month(jan) --> "Jan".
month(feb) --> "Feb".
month(mar) --> "Mar".
month(apr) --> "Apr".
month(may) --> "May".
month(jun) --> "Jun".
month(jul) --> "Jul".
month(aug) --> "Aug".
month(sep) --> "Sep".
month(oct) --> "Oct".
month(nov) --> "Nov".
month(dec) --> "Dec".

time(time(H, M, S)) -->
    integer(H), ":", integer(M), ":", integer(S).

timezone(edt) --> "EDT".
timezone(est) --> "EST".
timezone(pdt) --> "PDT".
timezone(pst) --> "PST".
timezone(utc) --> "UTC".
timezone(gmt) --> "GMT".

/*
?- phrase(datetimestamp(Date), "Mon Jun 23 18:29:18 EDT 2025").
*/