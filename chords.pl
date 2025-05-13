% chords.pl
% Entry point: get notes from chord string (e.g., "Cm7")
notes_for_chord(ChordStr, Notes) :-
    parse_chord(ChordStr, Root, Type),
    notes_for_chord(Root, Type, RawNotes),
    maplist(to_preferred_note, RawNotes, Notes).

% Main interactive loop
main :-
    writeln('Welcome to Chord Notes! This program was written by Joe Legner with help from ChatGPT on April 18, 2025.'),
    run_tests,
    writeln('Type `help` for help or enter a chord name.'),
    repeat,
    prompt_for_chord,
    fail.

% Runs the tests and outputs their results
run_tests :-
    test_major,
    test_minor,
    test_augmented,
    test_diminished,
    test_major7,
    test_minor7,
    test_dominant7,
    test_major9,
    test_minor9,
    test_dominant9,
    test_diminished9,
    test_sus2,
    test_sus4.

% Prompt the user for a chord and display the notes or help
prompt_for_chord :-
    write('Chord name: '),
    read_line_to_string(user_input, Input),
    (   Input == end_of_file
    ->  halt
    ;   normalize_space(string(Trimmed), Input),
        (   Trimmed = "help"
        ->  print_help
        ;   notes_for_chord(Trimmed, Notes),
            format('Notes in ~w: ~w~n', [Trimmed, Notes])
        ),
        fail
    ).

% Help message
print_help :-
    writeln('--- Help: Enter a chord to get its notes ---'),
    writeln('Supported chord formats include:'),
    writeln('  C       (C major)'),
    writeln('  Dm      (D minor)'),
    writeln('  G7      (G dominant 7)'),
    writeln('  Fmaj7   (F major 7)'),
    writeln('  Ddim    (D diminished)'),
    writeln('  D+      (D augmented)'),
    writeln('  Csus2   (C suspended 2)'),
    writeln('  A9      (A dominant 9)'),
    writeln('Type Ctrl+D to exit.').

% Individual tests
test_major :-
    notes_for_chord('C', major, Notes),
    (Notes == ['C', 'E', 'G'] -> writeln('Test passed: C major'); writeln('Test failed: C major')).

test_minor :-
    notes_for_chord('D', minor, Notes),
    (Notes == ['D', 'F', 'A'] -> writeln('Test passed: D minor'); writeln('Test failed: D minor')).

test_augmented :-
    notes_for_chord('F', augmented, Notes),
    (Notes == ['F', 'A', 'Db'] -> writeln('Test passed: F augmented'); writeln('Test failed: F augmented')).

test_diminished :-
    notes_for_chord('G', diminished, Notes),
    (Notes == ['G', 'Bb', 'D'] -> writeln('Test passed: G diminished'); writeln('Test failed: G diminished')).

test_major7 :-
    notes_for_chord('C', major7, Notes),
    (Notes == ['C', 'E', 'G', 'B'] -> writeln('Test passed: C major7'); writeln('Test failed: C major7')).

test_minor7 :-
    notes_for_chord('A', minor7, Notes),
    (Notes == ['A', 'C', 'E', 'G'] -> writeln('Test passed: A minor7'); writeln('Test failed: A minor7')).

test_dominant7 :-
    notes_for_chord('G', dominant7, Notes),
    (Notes == ['G', 'B', 'D', 'F'] -> writeln('Test passed: G dominant7'); writeln('Test failed: G dominant7')).

test_major9 :-
    notes_for_chord('D', major9, Notes),
    (Notes == ['D', 'F#', 'A', 'C#', 'E'] -> writeln('Test passed: D major9'); writeln('Test failed: D major9')).

test_minor9 :-
    notes_for_chord('E', minor9, Notes),
    (Notes == ['E', 'G', 'B', 'D', 'F#'] -> writeln('Test passed: E minor9'); writeln('Test failed: E minor9')).

test_dominant9 :-
    notes_for_chord('A', dominant9, Notes),
    (Notes == ['A', 'C#', 'E', 'G', 'B'] -> writeln('Test passed: A dominant9'); writeln('Test failed: A dominant9')).

test_diminished9 :-
    notes_for_chord('B', diminished9, Notes),
    (Notes == ['B', 'D', 'F', 'Ab', 'C'] -> writeln('Test passed: B diminished9'); writeln('Test failed: B diminished9')).

test_sus2 :-
    notes_for_chord('D', sus2, Notes),
    (Notes == ['D', 'E', 'A'] -> writeln('Test passed: D sus2'); writeln('Test failed: D sus2')).

test_sus4 :-
    notes_for_chord('G', sus4, Notes),
    (Notes == ['G', 'C', 'D'] -> writeln('Test passed: G sus4'); writeln('Test failed: G sus4')).

% Parsing and chord calculation
parse_chord(Input, RootNote, ChordType) :-
    string_chars(Input, Chars),
    parse_root(Chars, RootStr, Rest),
    atom_string(RootNoteAtom, RootStr),
    normalize_root(RootNoteAtom, RootNote),
    parse_type(Rest, ChordType).

parse_root([L1], RootStr, []) :- string_chars(RootStr, [L1]).
parse_root([L1, L2 | Rest], RootStr, Rest) :-
    member(L2, ['b', '#']),
    string_chars(RootStr, [L1, L2]).
parse_root([L1 | Rest], RootStr, Rest) :-
    \+ member(L1, ['b', '#']),
    string_chars(RootStr, [L1]).

normalize_root(In, Out) :-
    (enharmonic(In, Enh) -> Out = Enh ; Out = In).

parse_type([], major).
parse_type(['m'], minor).
parse_type(['m','7'], minor7).
parse_type(['m','9'], minor9).
parse_type(['m','a','j','7'], major7).
parse_type(['m','a','j','9'], major9).
parse_type(['d','i','m'], diminished).
parse_type(['d','i','m','7'], diminished7).
parse_type(['d','i','m','9'], diminished9).
parse_type(['7'], dominant7).
parse_type(['9'], dominant9).
parse_type(['+'], augmented).
parse_type(['s','u','s','2'], sus2).
parse_type(['s','u','s','4'], sus4).
parse_type(_, unknown).

% Define the formula for each chord type (intervals)
chord_formula(major, [0, 4, 7]).
chord_formula(minor, [0, 3, 7]).
chord_formula(augmented, [0, 4, 8]).
chord_formula(diminished, [0, 3, 6]).
chord_formula(major7, [0, 4, 7, 11]).
chord_formula(minor7, [0, 3, 7, 10]).
chord_formula(dominant7, [0, 4, 7, 10]).
chord_formula(major9, [0, 4, 7, 11, 14]).
chord_formula(minor9, [0, 3, 7, 10, 14]).
chord_formula(dominant9, [0, 4, 7, 10, 14]).
chord_formula(diminished9, [0, 3, 6, 8, 10]).
chord_formula(sus2, [0, 2, 7]).
chord_formula(sus4, [0, 5, 7]).

% Semitone scale (C-major style)
semitone_order(['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B']).

% Enharmonic input normalization
enharmonic('Db', 'C#').
enharmonic('Eb', 'D#').
enharmonic('Gb', 'F#').
enharmonic('Ab', 'G#').
enharmonic('Bb', 'A#').

% Enharmonic output preference: prefer flats
to_preferred_note('A#', 'Bb').
to_preferred_note('Db', 'C#').
to_preferred_note('D#', 'Eb').
to_preferred_note('F#', 'Gb').
to_preferred_note('Ab', 'G#').
to_preferred_note(Note, Note).

% Chord calculation
notes_for_chord(Root, Type, Notes) :-
    semitone_order(Scale),
    chord_formula(Type, Intervals),
    note_index(Root, Scale, Index),
    notes_from_intervals(Scale, Index, Intervals, Notes).

note_index(Note, Scale, Index) :-
    nth0(Index, Scale, Note), !.

notes_from_intervals(_, _, [], []).
notes_from_intervals(Scale, RootIndex, [I|Is], [Note|Ns]) :-
    Index is (RootIndex + I) mod 12,
    nth0(Index, Scale, Note),
    notes_from_intervals(Scale, RootIndex, Is, Ns).

% --- transpose predicates ---
transpose(ChordStrs, Semitones, TransposedChordStrs) :-
    maplist(transpose_chord(Semitones), ChordStrs, TransposedChordStrs).

transpose_chord(Semitones, ChordStr, TransposedStr) :-
    parse_chord(ChordStr, Root, Type),
    semitone_order(Scale),
    note_index(Root, Scale, Index),
    TransposedIndex is (Index + Semitones) mod 12,
    nth0(TransposedIndex, Scale, TransposedRoot),
    chord_suffix(Type, Suffix),
    string_concat(TransposedRoot, Suffix, TransposedStr).

chord_suffix(major, "").
chord_suffix(minor, "m").
chord_suffix(minor7, "m7").
chord_suffix(minor9, "m9").
chord_suffix(major7, "maj7").
chord_suffix(major9, "maj9").
chord_suffix(diminished, "dim").
chord_suffix(diminished7, "dim7").
chord_suffix(diminished9, "dim9").
chord_suffix(dominant7, "7").
chord_suffix(dominant9, "9").
chord_suffix(augmented, "+").
chord_suffix(sus2, "sus2").
chord_suffix(sus4, "sus4").
chord_suffix(unknown, "").

print_chords(ChordList) :-
    atomic_list_concat(ChordList, ' ', ChordsAtom),
    format('"~w".~n', [ChordsAtom]).
