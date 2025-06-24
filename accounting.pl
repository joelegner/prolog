% accounting.pl
% This is an accounting proof of concept.
% Here is the structure of the data. It was adapted from a (much longer) spreadsheet.
show(
    year_month_day(2025, 04, 20),
    venue(escape),
    pay(base(56250), tips(cash(8800), venmo(0)))
).

show(
    year_month_day(2025, 04, 22),
    venue(escape),
    pay(base(56250), tips(cash(9300), venmo(0)))
).

show(
    year_month_day(2025, 04, 24),
    venue(escape),
    pay(base(56250), tips(cash(9700), venmo(0)))
).

show(
    year_month_day(2025, 04, 26),
    venue(escape),
    pay(base(56250), tips(cash(6000), venmo(0)))
).

% Rule to extract cash tips
cash_tip(Tip) :-
    show(_, _, pay(_, tips(cash(Tip), _))).

% Get all show dates
all_show_dates(Dates) :-
    findall(Date, show(Date, _, _), Dates).

% Get the first and last date (chronologically)
start_date_end_date(StartDate, EndDate) :-
    all_show_dates(Dates),
    sort(Dates, SortedDates),
    SortedDates = [StartDate|_],
    append(_, [EndDate], SortedDates).

% Count number of shows
number_of_shows(Count) :-
    findall(1, cash_tip(_), List),
    length(List, Count).

% Total cash tips
total_cash_tips(Total) :-
    findall(Tip, cash_tip(Tip), Tips),
    sum_list(Tips, Total).

% Average cash tips
average_cash_tips(Average) :-
    total_cash_tips(Total),
    number_of_shows(Count),
    Count > 0,
    Average is Total / Count.

% Helper: format a date as M/D/YYYY
year_month_day_string(year_month_day(Year, Month, Day), String) :-
    format(atom(String), '~w/~w/~w', [Month, Day, Year]).

cents_currency_string(Cents, CurrencyString) :-
    integer(Cents),
    Dollars is 1.0*Cents/100.0,
    format(atom(CurrencyString), '$~2f', [Dollars]).

% Print the full report directly
print_cash_tips_report :-
    number_of_shows(Count),
    start_date_end_date(Start, End),
    total_cash_tips(Total),
    average_cash_tips(Average),
    year_month_day_string(Start, StartStr),
    year_month_day_string(End, EndStr),
    cents_currency_string(Total, TotalStr),
    cents_currency_string(Average, AverageStr),
    format('For ~w shows from ~w to ~w, cash tips totaled ~w for an average of ~w per show.~n',
           [Count, StartStr, EndStr, TotalStr, AverageStr]).

% And it worked!
%
% ?- print_cash_tips_report.
% For 4 shows from 4/20/2025 to 4/26/2025, cash tips totaled $338.00 for an average of $84.50 per show.