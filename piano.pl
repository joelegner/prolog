:- module(piano, [key/3, major_triad/2, print_notes/1, print_inversions/1]).

% Declare all 88 keys on a piano keyboard.
% key(Number, AtomName, Text).
key(1,  a0,  "A0").
key(2,  bb0, "Bb0").
key(3,  b0,  "B0").
key(4,  c1,  "C1").
key(5,  cs1, "C#1").
key(6,  d1,  "D1").
key(7,  ds1, "D#1").
key(8,  e1,  "E1").
key(9,  f1,  "F1").
key(10, fs1, "F#1").
key(11, g1,  "G1").
key(12, gs1, "G#1").
key(13, a1,  "A1").
key(14, bb1, "Bb1").
key(15, b1,  "B1").
key(16, c2,  "C2").
key(17, cs2, "C#2").
key(18, d2,  "D2").
key(19, ds2, "D#2").
key(20, e2,  "E2").
key(21, f2,  "F2").
key(22, fs2, "F#2").
key(23, g2,  "G2").
key(24, gs2, "G#2").
key(25, a2,  "A2").
key(26, bb2, "Bb2").
key(27, b2,  "B2").
key(28, c3,  "C3").
key(29, cs3, "C#3").
key(30, d3,  "D3").
key(31, ds3, "D#3").
key(32, e3,  "E3").
key(33, f3,  "F3").
key(34, fs3, "F#3").
key(35, g3,  "G3").
key(36, gs3, "G#3").
key(37, a3,  "A3").
key(38, bb3, "Bb3").
key(39, b3,  "B3").
key(40, c4,  "C4").
key(41, cs4, "C#4").
key(42, d4,  "D4").
key(43, ds4, "D#4").
key(44, e4,  "E4").
key(45, f4,  "F4").
key(46, fs4, "F#4").
key(47, g4,  "G4").
key(48, gs4, "G#4").
key(49, a4,  "A4").
key(50, bb4, "Bb4").
key(51, b4,  "B4").
key(52, c5,  "C5").
key(53, cs5, "C#5").
key(54, d5,  "D5").
key(55, ds5, "D#5").
key(56, e5,  "E5").
key(57, f5,  "F5").
key(58, fs5, "F#5").
key(59, g5,  "G5").
key(60, gs5, "G#5").
key(61, a5,  "A5").
key(62, bb5, "Bb5").
key(63, b5,  "B5").
key(64, c6,  "C6").
key(65, cs6, "C#6").
key(66, d6,  "D6").
key(67, ds6, "D#6").
key(68, e6,  "E6").
key(69, f6,  "F6").
key(70, fs6, "F#6").
key(71, g6,  "G6").
key(72, gs6, "G#6").
key(73, a6,  "A6").
key(74, bb6, "Bb6").
key(75, b6,  "B6").
key(76, c7,  "C7").
key(77, cs7, "C#7").
key(78, d7,  "D7").
key(79, ds7, "D#7").
key(80, e7,  "E7").
key(81, f7,  "F7").
key(82, fs7, "F#7").
key(83, g7,  "G7").
key(84, gs7, "G#7").
key(85, a7,  "A7").
key(86, bb7, "Bb7").
key(87, b7,  "B7").
key(88, c8,  "C8").

note_index(Note, Index) :- key(Index, Note, _).

major_triad(Root, [Root, Third, Fifth]) :-
    note_index(Root, RootIndex),
    ThirdIndex is RootIndex + 4,
    FifthIndex is RootIndex + 7,
    key(ThirdIndex, Third, _),
    key(FifthIndex, Fifth, _).

print_notes([]).
print_notes([Note | Notes]) :-
    ( key(_, Note, Text) ->
        writeln(Text)
    ; format("Unknown note: ~w~n", [Note])
    ),
    print_notes(Notes).

% All inversions of a major triad
major_triad_inversions(Root, Inversions) :-
    major_triad(Root, [N1, N2, N3]),
    Inversions = [
        [N1, N2, N3],  % root position
        [N2, N3, N1],  % 1st inversion
        [N3, N1, N2]   % 2nd inversion
    ].

print_inversions(Root) :-
    major_triad_inversions(Root, Inversions),
    print_inversions_with_separator(Inversions).

print_inversions_with_separator([]).
print_inversions_with_separator([Chord | Rest]) :-
    print_inversion(Chord),
    writeln("------------"),
    print_inversions_with_separator(Rest).

print_inversion(Chord) :-
    maplist(print_note, Chord),
    nl.

print_note(Note) :-
    ( key(_, Note, Text) ->
        write(Text), write(" ")
    ; format("Unknown note: ~w ", [Note])
    ).