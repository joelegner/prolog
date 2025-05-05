:- module(axiomatic_design, [
    funtional_requirement/1,
    design_parameter/1,
    depends_on/2,
    zig/2,
    zag/2,
    need_to_zag/1,
    parent/2,
    all_need_to_zag/1,
    print_need_to_zig_snippets/0,
    print_need_to_zag_snippets/0
    ]).

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

print_need_to_zig_snippets :-
    all_need_to_zig(FRs),
    forall(member(FR, FRs), print_zig_template(FR)).

print_zig_template(FR) :-
    format('% ~w zig-zag~n', [FR]),
    format('zig(~w, replace_with_design_parameter).~n~n', [FR]).

print_need_to_zag_snippets :-
    all_need_to_zag(DPs),
    forall(member(DP, DPs), print_zag_template(DP)).

print_zag_template(DP) :-
    format('% ~w zig-zag~n', [DP]),
    format('% TODO: zag/2~n~n').

% Start zig-zagging ========================================================

% Zig-Zag Process from Axiomatic Design (AD) by Nam P. Suh.
% 1. Start with FR.
% 2. Zig to create one new DP for FR.
% 3. Zag to create n new FRs for DP.
% 4. For each of n new FRs: go to 1.

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

% publish_documentation zig-zag
zig(publish_documentation, documentation_development_system).
% TODO: zag/2

% view_documentation zig-zag
zig(view_documentation, documentation_website).
% TODO: zag/2

% upload_app zig-zag
zig(upload_app, app_store_upload_process).
% TODO: zag/2

% approve_app zig-zag
zig(approve_app, app_store_approval_process).
% TODO: zag/2

% publish_app zig-zag
zig(publish_app, xcode_publish_procedure).
% TODO: zag/2

% install_app zig-zag
zig(install_app, app_store_app).
% TODO: zag/2

% capture_bugs zig-zag
zig(capture_bugs, bug_collection_system).

% debug_code zig-zag
zig(debug_code, xcode_debug_system).

% develop_app zig-zag
zig(develop_app, development_system).

% find_app zig-zag
zig(find_app, app_store_search_feature).

% host_files zig-zag
zig(host_files, github).

% manage_versioning zig-zag
zig(manage_versioning, git).

% push_update zig-zag
zig(push_update, git_push_command).

% tap_and_hold zig-zag
zig(tap_and_hold, app_icon).

% tap_delete zig-zag
zig(tap_delete, ios_delete_app_feature).

% documentation_development_system zig-zag
% TODO: zag/2

% documentation_website zig-zag
% TODO: zag/2

% app_store_upload_process zig-zag
% TODO: zag/2

% app_store_approval_process zig-zag
% TODO: zag/2

% xcode_publish_procedure zig-zag
% TODO: zag/2

% app_store_app zig-zag
% TODO: zag/2

% bug_collection_system zig-zag
% TODO: zag/2

% xcode_debug_system zig-zag
% TODO: zag/2

% development_system zig-zag
% TODO: zag/2

% app_store_search_feature zig-zag
% TODO: zag/2

% github zig-zag
% TODO: zag/2

% git zig-zag
% TODO: zag/2

% git_push_command zig-zag
% TODO: zag/2

% app_icon zig-zag
% TODO: zag/2

% ios_delete_app_feature zig-zag
% TODO: zag/2