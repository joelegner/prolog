% Your facts
provides(dp(desk), fr(provides_desk_functionality)).
provides(dp(desktop), fr(provide_working_surface)).
provides(dp(leg_assembly), fr(support_working_surface)).

provides(dp(wood_panel), fr(provide_writing_surface)).
provides(dp(drilled_holes), fr(attach_to_legs)).
provides(dp(edge_treatment), fr(provide_comfort)).

provides(dp(wood), fr(act_as_panel)).
provides(dp(wood_finish_system), fr(protect_wood)).

parts(desk, [desktop, leg_assembly]).
parts(desktop, [wood_panel, drilled_holes, edge_treatment]).
parts(wood_panel, [wood, wood_finish_system]).

% Recursive hierarchy printing
print_hierarchy :-
    print_dp(dp(desk), 0).

print_dp(DP, Indent) :-
    provides(DP, FR),
    DP = dp(DPName),
    FR = fr(FRName),
    tab(Indent), format("~w provides ~w~n", [DPName, FRName]),
    ( parts(DPName, SubParts) ->
        NextIndent is Indent + 2,
        print_subparts(SubParts, NextIndent)
    ; true ).

print_subparts([], _).
print_subparts([SubPart|Rest], Indent) :-
    print_dp(dp(SubPart), Indent),
    print_subparts(Rest, Indent).

% ?- print_hierarchy.
% desk provides provide_desk_functionality
%   desktop provides provide_working_surface
%     wood_panel provides provide_writing_surface
%       wood provides act_as_panel
%       wood_finish_system provides protect_wood
%     drilled_holes provides attach_to_legs
%     edge_treatment provides provide_comfort
%   leg_assembly provides support_working_surface
% true.

% Let's define the physicality of the system
% A design parameter is a physical variable.
physical(X) :- provides(dp(X), _).

all_physical_objects(List) :-
    findall(X, physical(X), List).
