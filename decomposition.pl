% Functional Requirements
% Level 0
fr(fr0, 'FR0', 'Kill it at Fluffernutters 2025').

% Level 1
fr(fr1, 'FR1', 'Travel there and back').
fr(fr2, 'FR2', 'Produce the shows').
fr(fr3, 'FR3', 'Perform shows').

% Level 2

% Design Parameters
% Level 0
dp(dp0, 'DP0', 'Fluffernutters 2025 Project').

% Level 1
dp(dp1, 'DP1', 'Travel Arrangements').
dp(dp2, 'DP2', 'Production System').
dp(dp3, 'DP3', 'Stage Setup'). 

% Level 2

% Relationships
link(fr0, dp0).
link(dp0, fr1).
link(fr1, dp1).
link(dp0, fr2).
link(fr2, dp2).
link(dp0, fr3).
link(fr3, dp3).

% Wrapping
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
    forall(fr(Id, Label, Desc),
        ( wrap_text(Desc, 24, WrappedDesc),
          escape_newlines(WrappedDesc, EscapedDesc),
          format('    ~w [label="~w\\n~w"];\n', [Id, Label, EscapedDesc]) )),
    forall(dp(Id, Label, Desc),
        ( wrap_text(Desc, 24, WrappedDesc),
          escape_newlines(WrappedDesc, EscapedDesc),
          format('    ~w [label="~w\\n~w"];\n', [Id, Label, EscapedDesc]) )),
    nl,
    forall(link(From, To),
        format('    ~w -> ~w;\n', [From, To])),
    writeln('}').
