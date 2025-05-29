% Functional Requirements
% Level 0
fr(fr0, 'FR0', 'Kill it at Hedonism II').

% Level 1
fr(fr1, 'FR1', 'Define and design a manufacturable product').
fr(fr2, 'FR2', 'Produce the designed product').
fr(fr3, 'FR3', 'Distribute the finished product').

% Level 2

% Design Parameters
% Level 0
dp(dp0, 'DP0', 'Corporate System').

% Level 1
dp(dp1, 'DP1', 'Product Definition system').
dp(dp2, 'DP2', 'Manufacturing System').
dp(dp3, 'DP3', 'Product Distribution System').

% Level 2

% Relationships
link(fr0, dp0).
link(dp0, fr1).
link(fr1, dp1).
link(dp0, fr2).
link(fr2, dp2).
link(dp0, fr3).
link(fr3, dp3).

% Main predicate to generate DOT code
main :-
    writeln('digraph AxiomaticDesign {'),
    writeln('    node [shape=box];'),
    nl,
    forall(fr(Id, Label, Desc),
        format('    ~w [label="~w\\n~w"];\n', [Id, Label, Desc])),
    forall(dp(Id, Label, Desc),
        format('    ~w [label="~w\\n~w"];\n', [Id, Label, Desc])),
    nl,
    forall(link(From, To),
        format('    ~w -> ~w;\n', [From, To])),
    writeln('}').
