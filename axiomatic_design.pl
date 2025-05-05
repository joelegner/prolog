:- module(axiomatic_design, [
    funtional_requirement/1,
    design_parameter/1,
    depends_on/2,
    zig/2,
    zag/2,
    need_to_zag/1,
    parent/2,
    all_need_to_zag/1,
    print_zig_templates/0,
    print_zag_templates/0,
    print_fr_hierarchy/0,
    print_dp_hierarchy/0,
    print_design_report/0,
    zigzag/0
    ]).

:- dynamic zig/2.
:- dynamic zag/2.

:- discontiguous depends_on/2.
:- discontiguous zag/2.
:- discontiguous zig/2.

% Categorize using zig =====================================================
funtional_requirement(X) :-
    zig(X, _).

design_parameter(X) :-
    zig(_, X).

% Link up DPs by parent and child relationships through shared FRs.
parent(P, C) :-
    zig(_, P),
    zag(P, FRs),
    member(FR, FRs),
    zig(FR, C).

% Holds when DP has not been split up into functional requirements
% using the zag/2 predicate.
need_to_zag(DP) :-          % DP needs to zag if:
    design_parameter(DP),   % DP is design parameter, and
    \+ zag(DP, _).          % DP has not zagged.

% Example of using findall/3:
% findall(Object,Goal,List).
% Find all: If Object satisfies Goal it is in the List.
% Below: If DP satisfies need_to_zag/1, it is in the list DPs.
all_need_to_zag(DPs) :-
     findall(DP, need_to_zag(DP), DPs).

% Holds when FR has not yet been assigned a design parameter using zig/2.
need_to_zig(FR) :-
    zag(_, FRs),       % For each DP's list of child FRs
    member(FR, FRs),   % Take each FR
    \+ zig(FR, _).     % That hasn't been zigged yet.

all_need_to_zig(FRs) :-
    setof(FR, need_to_zig(FR), FRs).

print_zig_templates :-
    all_need_to_zig(FRs),
    forall(member(FR, FRs), print_zig_template(FR)).

print_zig_template(FR) :-
    format('% ~w zig-zag~n', [FR]),
    format('zig(~w, replace_with_design_parameter).~n', [FR]),
    format('% TODO: zag/2~n~n').

print_zag_templates :-
    all_need_to_zag(DPs),
    forall(member(DP, DPs), print_zag_template(DP)).

print_zag_template(DP) :-
    format('% ~w zig-zag~n', [DP]),
    format('zag(~w, [ ]).~n', [DP]),
    format('% TODO: zig/2~n~n').

% Print FR hierarchy ========================================================

% Top-level FRs: Those that appear in zig/2 but not in any zag/2 child list.
top_level_fr(FR) :-
    zig(FR, _),
    \+ (zag(_, FRs), member(FR, FRs)).

% Get child FRs of a given FR through zig-zag
child_frs(FR, ChildFRs) :-
    zig(FR, DP),
    zag(DP, ChildFRs), !.
child_frs(_, []).  % If no zag, no children.

% Print FR with indentation
print_fr(FR, Indent) :-
    tab(Indent),
    format('~w~n', [FR]),
    NextIndent is Indent + 4,
    child_frs(FR, Children),
    forall(member(C, Children), print_fr(C, NextIndent)).

% Entry point
print_fr_hierarchy :-
    forall(top_level_fr(FR), print_fr(FR, 0)).

% Print DP hierarchy ========================================================

% Top-level DPs: appear in zig/2 but not as children of any other DP
top_level_dp(DP) :-
    design_parameter(DP),
    \+ parent(_, DP).

% Get child DPs of a given DP through shared FRs
child_dps(DP, Children) :-
    zag(DP, FRs),
    findall(ChildDP,
        (member(FR, FRs), zig(FR, ChildDP)),
        Children), !.
child_dps(_, []).

% Print DP with indentation
print_dp(DP, Indent) :-
    tab(Indent),
    format('~w~n', [DP]),
    NextIndent is Indent + 4,
    child_dps(DP, Children),
    forall(member(C, Children), print_dp(C, NextIndent)).

% Entry point
print_dp_hierarchy :-
    forall(top_level_dp(DP), print_dp(DP, 0)).

% Print design report ========================================================

print_design_report :-
    print_fr_hierarchy,
    nl,
    print_dp_hierarchy,
    nl.

% Start zig-zagging ========================================================

% Zig-Zag Process from Axiomatic Design (AD) by Nam P. Suh.
% 1. Start with FR.
% 2. Zig to create one new DP for FR.
% 3. Zag to create n new FRs for DP.
% 4. For each of n new FRs: go to 1.

zigzag :-
    all_need_to_zig(FRs),
    forall(member(FR, FRs), process_zigzag(FR)).

process_zigzag(FR) :-
    format('Name of DP for FR "~w"? ', [FR]),
    read(DP),
    format('zig(~w, ~w).~n', [FR, DP]),
    format('zag(~w, []).~n', [DP]),
    assertz(zig(FR, DP)),
    assertz(zag(DP, [])).

% First zig-zag
zig(make_dueling_pianos_fun, dueler_mvp_system). % Note 1
zag(dueler_mvp_system, [learn, install, run, update, uninstall]). % Note 3

% learn zig-zag
zig(learn, documentation_system).
zag(documentation_system, [publish_documentation, view_documentation]).
depends_on(view_documentation, publish_documentation).

% install zig-zag
zig(install, app_store_interface).
zag(app_store_interface, [upload_app, approve_app, publish_app, install_app]).
depends_on(approve_app, upload_app).
depends_on(publish_app, approve_app).
depends_on(install, approve_app).

% run zig-zag
zig(run, dueler_mvp_app).
zag(dueler_mvp_app, [host_files, manage_versioning, develop_app]).

% update zig-zag
zig(update, update_system).
zag(update_system, [capture_bugs, debug_code, push_update]).
depends_on(debug_code, capture_bugs).
depends_on(push_update, debug_code).

% uninstall zig-zag
zig(uninstall, ios_uninstall_procedure).
zag(ios_uninstall_procedure, [find_app, tap_and_hold, tap_delete]).

% approve_app zig-zag
zig(approve_app, app_store_approval_system).
zag(app_store_approval_system, []).
