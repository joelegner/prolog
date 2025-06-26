% serial_number.pl
% Generate miniature UUID-like identifiers using `joe sn` shell command.

:- module(serial_number, [
    serial_number/1,
    run/0
    ]).

:- use_module(shell_output).

serial_number(SN) :-
    shell_output_atom('joe sn', SN).

run :-
    serial_number(SN),
    writeln(SN).
