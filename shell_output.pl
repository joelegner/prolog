% shell_output.pl

:- module(shell_output, [
    shell_output_atom/2,
    shell_output_string/2,
    trim/2
    ]).

:- use_module(library(dcg/basics)).  % For `blanks//0`, `string//1`, etc.

% DCG that removes leading/trailing whitespace including newlines
trimmed(S) --> blanks, string(S), blanks, eos.

% Trim whitespace from both ends of a string
trim(S, T) :-
    string_codes(S, Codes),
    phrase(trimmed(TrimmedCodes), Codes),
    string_codes(T, TrimmedCodes).

% Run shell command and return trimmed output as a string
shell_output_string(Command, Output) :-
    process_create(path(sh), ['-c', Command],
                   [stdout(pipe(Out)), process(PID)]),
    read_string(Out, _, RawOutput),
    close(Out),
    process_wait(PID, _),
    once(trim(RawOutput, Output)).

% Run shell command and return trimmed output as an atom
shell_output_atom(Command, Output) :-
    shell_output_string(Command, StringOutput),
    atom_string(Output, StringOutput).
