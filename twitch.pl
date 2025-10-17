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

% Top Level
fr_name_cns(fr0, 'Make first $1 streaming Angband on Twitch', [cn1]).
dp_name(dp0, Name) :- production_system_acronym(dragon, Name).

% Start zig-zagging
fr_zig_dps(fr0, [dp0]).

parent_fr_child_fr_name(f0, fr1, 'Design the DRAGON system').
parent_fr_child_fr_name(f0, fr2, 'Run the business side').
parent_fr_child_fr_name(f0, fr3, 'Produce Twitch streams').

fr(FR) :- 
    fr_name_cns(FR, _, _).
fr(FR) :-
    parent_fr_child_fr_name(_, FR, _).

fr_name(FR, Name) :-
    fr_name_cns(FR, Name, _).
fr_name(FR, Name) :-
    parent_fr_child_fr_name(_, FR, Name).

fr_upper(FR, FRUpper) :-
    atom_string(FR, FRStr),               % Convert atom to string
    string_upper(FRStr, FRUpper).

format_fr(FR) :- 
    fr_name(FR, FRName),
    fr_upper(FR, FRUpper),
    format('~w: ~w~n', [FRUpper, FRName]).

frs(FRs) :- findall(FR, fr(FR), FRs).

dp_zag_frs(dp0, [fr1, fr2, fr3]).
