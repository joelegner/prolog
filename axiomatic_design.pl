:- discontiguous depends_on/2.
:- discontiguous zag/2.
:- discontiguous zig/2.

% First zig-zag
zig(make_dueling_pianos_fun, dueler_mvp_system).
zag(dueler_mvp_system, [learn, install, run, update, uninstall]).

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

% view_documentation zig-zag
zig(view_documentation, documentation_website).

% upload_app zig-zag
zig(upload_app, app_store_upload_process).

% approve_app zig-zag
zig(approve_app, app_store_approval_process).

% publish_app zig-zag
zig(publish_app, xcode_publish_procedure).

% install_app zig-zag
zig(install_app, app_store_app).

% Link up DPs by parent and child relationships through shared FRs.
parent(P, C) :-
    zig(_, P),
    zag(P, FRs),
    member(FR, FRs),
    zig(FR, C).
