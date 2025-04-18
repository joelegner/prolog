:- initialization(main).

% Root notes in semitone order (for building intervals)
semitone_order(['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B']).
enharmonic('Db', 'C#').
enharmonic('Eb', 'D#').
enharmonic('Gb', 'F#').
enharmonic('Ab', 'G#').
enharmonic('Bb', 'A#').

% Interval formulas (in semitone steps from root)
chord_formula(major,        [0, 4, 7]).
chord_formula(minor,        [0, 3, 7]).
chord_formula(augmented,    [0, 4, 8]).
chord_formula(diminished,   [0, 3, 6]).
chord_formula(major7,       [0, 4, 7, 11]).
chord_formula(minor7,       [0, 3, 7, 10]).
chord_formula(diminished7,  [0, 3, 6, 9]).
chord_formula(dominant7,    [0, 4, 7, 10]).

% Entry point
main :-
    write('Enter chord name (e.g., C, Dm, G7, Fmaj7, Ddim, D+): '),
    read_line_to_string(user_input, Input),
    parse_chord(Input, Root, Type),
    notes_for_chord(Root, Type, Notes),
    format('Notes in ~w ~w: ~w~n', [Root, Type, Notes]),
    halt.

% Parses input like "C", "Dm", "Fmaj7", "G+" into root and type
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

% Normalize flat notes to sharps for consistent lookup
normalize_root(In, Out) :-
    (enharmonic(In, Enh) -> Out = Enh ; Out = In).

parse_type([], major).
parse_type(['m'], minor).
parse_type(['m', '7'], minor7).
parse_type(['m', 'a', 'j', '7'], major7).
parse_type(['7'], dominant7).
parse_type(['d', 'i', 'm'], diminished).
parse_type(['+'], augmented).
parse_type(['d', 'i', 'm', '7'], diminished7).
parse_type(_, unknown).

% Get chord notes by applying formula to root
notes_for_chord(Root, Type, Notes) :-
    semitone_order(Scale),
    chord_formula(Type, Intervals),
    note_index(Root, Scale, Index),
    notes_from_intervals(Scale, Index, Intervals, Notes).

note_index(Note, Scale, Index) :-
    nth0(Index, Scale, Note), !.

% Wrap-around semitone math
notes_from_intervals(_, _, [], []).
notes_from_intervals(Scale, RootIndex, [I|Is], [Note|Ns]) :-
    Index is (RootIndex + I) mod 12,
    nth0(Index, Scale, Note),
    notes_from_intervals(Scale, RootIndex, Is, Ns).
