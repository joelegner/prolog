% Functional Requirements
% Level 0
fr(fr0, 'Kill it at Fluffernutters 2025').

% FR0 Level 1
fr(fr1, 'Travel there and back').
fr(fr2, 'Produce the shows').
fr(fr3, 'Perform shows').

% FR1 Level 2
fr(fr11, 'Prepare to travel').
fr(fr12, 'Travel to Fluffernutters').
fr(fr13, 'Travel back home').

% FR3 Level 2
fr(fr31, 'Produce sound and light').
fr(fr32, 'Play music and sing').

% FR11 Level 3
fr(fr111, 'Document what is needed').
fr(fr112, 'Make sure everything gets packed').
fr(fr113, 'Contain and carry things').

% Design Parameters
% Level 0
dp(dp0, 'Fluffernutters 2025 Project').

% DP0 Level 1
dp(dp1, 'Travel Arrangements').
dp(dp2, 'Production System').
dp(dp3, 'Stage Setup').

% DP1 Level 2
dp(dp11, 'Packing System').
dp(dp12, 'Travel Day 1').
dp(dp13, 'Travel Day 2').

% DP3 Level 2
dp(dp31, 'Sound system').
dp(dp32, 'The Wonder Twins').

% DP11 Level 3
dp(dp111, 'Apple Reminders Fluffernutters 2025 note').
dp(dp112, 'Packing checklist instance').
dp(dp113, 'Luggage and travel equipment and supplies').

% Relationships
link(fr0, dp0).
link(dp0, fr1).
link(fr1, dp1).
link(dp0, fr2).
link(fr2, dp2).
link(dp0, fr3).
link(fr3, dp3).

% DP1 Level 2
link(dp1, fr11).
link(dp1, fr12).
link(dp1, fr13).
link(fr11, dp11).
link(fr12, dp12).
link(fr13, dp13).

% DP11 Level 3
link(dp11, fr111).
link(dp11, fr112).
link(dp11, fr113).
link(fr111, dp111).
link(fr112, dp112).
link(fr113, dp113).

% DP2 Level 2

% DP3 Level 2
link(dp3, fr31).
link(dp3, fr32).
link(fr31, dp31).
link(fr32, dp32).

% Wrapping utility
wrap_text(Text, Width, Wrapped) :-
    split_string(Text, " ", "", Words),
    wrap_words(Words, Width, "", [], Lines),
    atomic_list_concat(Lines, '\n', Wrapped).

wrap_words([], _, Line, Acc, Lines) :-
    ( Line \= "" -> append(Acc, [Line], Lines)
    ; Lines = Acc ).
wrap_words([Word|Rest], Width, Line, Acc, Lines) :-
    ( Line = "" -> NewLine = Word
    ; atomics_to_string([Line, Word], " ", Temp),
      string_length(Temp, Len),
      ( Len =< Width -> NewLine = Temp, NewAcc = Acc
      ; append(Acc, [Line], NewAcc), NewLine = Word )
    ),
    wrap_words(Rest, Width, NewLine, NewAcc, Lines).

escape_newlines(Input, Escaped) :-
    split_string(Input, "\n", "", Parts),
    atomic_list_concat(Parts, '\\n', Escaped).

% Main predicate to generate DOT code
main :-
    writeln('digraph AxiomaticDesign {'),
    writeln('    node [shape=box width=1];'),
    nl,
    forall(fr(Id, Desc),
        ( atom_string(Id, AtomStr),
          string_upper(AtomStr, Label),
          wrap_text(Desc, 24, WrappedDesc),
          escape_newlines(WrappedDesc, EscapedDesc),
          format('    ~w [label="~w\\n~w"];\n', [Id, Label, EscapedDesc]) )),
    forall(dp(Id, Desc),
        ( atom_string(Id, AtomStr),
          string_upper(AtomStr, Label),
          wrap_text(Desc, 24, WrappedDesc),
          escape_newlines(WrappedDesc, EscapedDesc),
          format('    ~w [label="~w\\n~w"];\n', [Id, Label, EscapedDesc]) )),
    nl,
    forall(link(From, To),
        format('    ~w -> ~w;\n', [From, To])),
    writeln('}').
