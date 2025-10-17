module(twitch, [
    todo/1
]).

% p9 is the project number.
project_name(       p9,     'P9: Joe Twitch Channel Project').
project_author(     p9,     'Joe Legner').
project_end(        p9,     'Make $1 on Twitch').
project_startdate(  p9,     date(2025, 10, 17)).

production_system(dragon).
production_system_acronym(dragon, 'DRAGON').
production_system_name(dragon, 'Dank Retro Angband Gaming Online Nexus').

project_production_system(p9, dragon). 

cn(cn1, 'Make $1 streaming Angband on Twitch').

% Top Level FR
fr_name_cns(fr0, 'Make first $1 streaming Angband on Twitch', [cn1]).

% Design Parameters (DP)
dp_name(dp0, Name) :- production_system_acronym(dragon, Name).
dp_name(dp1, 'DRAGON Design System').
dp_name(dp2, 'DRAGON Business System').
dp_name(dp3, 'DRAGON Creative System').

% Start zig-zagging
fr_zig_dps(fr0, [dp0]).
fr_zig_dps(fr1, [dp1]).
fr_zig_dps(fr2, [dp2]).
fr_zig_dps(fr3, [dp3]).

parent_fr_child_fr_name(f0, fr1, 'Design the DRAGON system').
parent_fr_child_fr_name(f0, fr2, 'Run the business side').
parent_fr_child_fr_name(f0, fr3, 'Produce Twitch streams').

fr_dp(FR, DP) :-
    fr_zig_dps(FR, DPs),
    member(DP, DPs).

fr(FR) :- 
    fr_name_cns(FR, _, _).
fr(FR) :-
    parent_fr_child_fr_name(_, FR, _).

fr_name(FR, Name) :-
    fr_name_cns(FR, Name, _).
fr_name(FR, Name) :-
    parent_fr_child_fr_name(_, FR, Name).

atom_upper(Atom, AtomUpper) :-
    atom_string(Atom, AtomString),               % Convert atom to string
    string_upper(AtomString, AtomUpper).

format_dp(DP) :- 
    atom_upper(DP, DPUpper),
    dp_name(DP, DPName),
    format('~w: ~w~n', [DPUpper, DPName]).

format_fr(FR) :- 
    atom_upper(FR, FRUpper),
    fr_name(FR, FRName),
    format('~w: ~w~n', [FRUpper, FRName]).

frs(FRs) :- findall(FR, fr(FR), FRs).

dp_zag_frs(dp0, [fr1, fr2, fr3]).

print_fr_dp(FR, DP) :-
    format_fr(FR),
    format_dp(DP),
    format('~n').

print_report :-
    format('FR-DP Pairs:~n~n'),
    forall(fr_dp(FR, DP), print_fr_dp(FR, DP)).