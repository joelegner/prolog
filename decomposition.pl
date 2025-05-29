% Functional Requirements
fr(fr0, 'FR0\nAchieve the corporate\nbusiness strategy plan').
fr(fr1, 'FR1\nDefine and design\na manufacturable\nproduct').
fr(fr2, 'FR2\nProduce the\ndesigned product').
fr(fr3, 'FR3\nDistribute the\nfinished product').

% Design Parameters
dp(dp0, 'DP0\nCorporate System').
dp(dp1, 'DP1\nProduct Definition\nsystem').
dp(dp2, 'DP2\nManufacturing\nSystem').
dp(dp3, 'DP3\nProduct\nDistribution System').

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
    forall(fr(Id, Label),
        format('    ~w [label="~w"];\n', [Id, Label])),
    forall(dp(Id, Label),
        format('    ~w [label="~w"];\n', [Id, Label])),
    nl,
    forall(link(From, To),
        format('    ~w -> ~w;\n', [From, To])),
    writeln('}').
