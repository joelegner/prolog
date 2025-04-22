:- module(timestamp, [print_timestamp/0]).

:- use_module(library(date)).

print_timestamp :-
    get_time(Stamp),
    format_time(string(ISO8601), '%FT%T%:z', Stamp),
    writeln(ISO8601).
