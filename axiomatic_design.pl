:- module(axiomatic_design, [
    funtional_requirement/1,
    design_parameter/1,
    depends_on/2,
    zig/2,
    zag/2,
    need_to_zag/1,
    parent/2,
    all_need_to_zag/1
]).

:- discontiguous depends_on/2.
:- discontiguous zag/2.
:- discontiguous zig/2.

% Categorize using zig =====================================================
funtional_requirement(X) :-
    zig(X, _).

design_parameter(X) :-
    zig(_, X).

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


% Link up DPs by parent and child relationships through shared FRs.
parent(P, C) :-
    zig(_, P),
    zag(P, FRs),
    member(FR, FRs),
    zig(FR, C).
