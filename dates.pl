%% dates.pl

/*
This is the default: 

:- set_prolog_flag(double_quotes, string).

But it does not seem to work. Usage example:

?- phrase(day_name(X), Phrase).
X = 0,
Phrase = [83, 117, 110] ;
X = 1,
Phrase = [77, 111, 110] ;
X = 2,
Phrase = [84, 117, 101] ;
X = 3,
Phrase = [87, 101, 100] ;
X = 4,
Phrase = [84, 104, 117] ;
X = 5,
Phrase = [70, 114, 105] ;
X = 6,
Phrase = [83, 97, 116] ;
X = 7,
Phrase = [83, 117, 110].

This one does not compile.

:- set_prolog_flag(double_quotes, atom).

This is close:

:- set_prolog_flag(double_quotes, chars).

?- phrase(day_name(X), Phrase).
X = 0,
Phrase = ['S', u, n] ;
X = 1,
Phrase = ['M', o, n] ;
X = 2,
Phrase = ['T', u, e] ;
X = 3,
Phrase = ['W', e, d] ;
X = 4,
Phrase = ['T', h, u] ;
X = 5,
Phrase = ['F', r, i] ;
X = 6,
Phrase = ['S', a, t] ;
X = 7,
Phrase = ['S', u, n]
*/

:- set_prolog_flag(double_quotes, string).

day_name(0) --> "Sun".
day_name(1) --> "Mon".
day_name(2) --> "Tue".
day_name(3) --> "Wed".
day_name(4) --> "Thu".
day_name(5) --> "Fri".
day_name(6) --> "Sat".
day_name(7) --> "Sun".

month_name(1) --> "Jan".
month_name(2) --> "Feb".
month_name(3) --> "Mar".
month_name(4) --> "Apr".
month_name(5) --> "May".
month_name(6) --> "Jun".
month_name(7) --> "Jul".
month_name(8) --> "Aug".
month_name(9) --> "Sep".
month_name(10) --> "Oct".
month_name(11) --> "Nov".
month_name(12) --> "Dec".

print_days :-
    forall(
        between(0, 7, X),
        (
            phrase(day_name(X), Codes),
            string_codes(Str, Codes),
            format("~w~n", [Str])
        )
    ).

print_months :-
    forall(
        between(1, 12, X),
        (
            phrase(month_name(X), Codes),
            string_codes(Str, Codes),
            format("~w~n", [Str])
        )
    ).

year_month_day_date(Y, M, D, date(Y, M, D)).
date_year(date(Y, _, _), Y).
date_month(date(_, M, _), M).
date_day(date(_, _, D), D).
