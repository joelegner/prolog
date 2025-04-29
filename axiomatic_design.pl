% ad stands for "Axiomatic Design" by Professor Nam P. Suh.
% ad(FR, DP, [CN]).
ad(make_dueling_more_fun, dueler_mvp_project, [have_fun, edit_database, find_songs, play_songs, sing_songs]).
ad(install_app, app_store_entry, [download, install, play_with]).
ad(run_app, dueler_mvp_app, [use_app]).

ad(develop_app, development_system, [use_app]).
ad(deploy_app, app_store_process, [download, install, play_with]).
ad(host_app_for_download, app_store, [discover_app, download, install, play_with]).

ad(interface_with_user, user_interface_code, [use_app, play_with]).
ad(manage_data, data_model_code, [find_songs, edit_database]).

children(dueler_mvp_project, [install_app, run_app]).
children(install_app, [develop_app, deploy_app, host_app_for_download]).
children(use_app, [interface_with_user, manage_data]).

% `Child` is a child of `Parent` if `Parent` has list `Children`, and
% `Child` is a member of list `Children`.
child(Child, Parent) :-
    children(Parent, Children),
    member(Child, Children).

% Works like child/2 but the operands are in reverse order. 
parent(Parent, Child) :-
    children(Parent, Children),
    member(Child, Children).

% A customer need, `CN`, is any member of the `CNs` in the `ad/3` structures.
need(CN) :-
    ad(_, _, CNs), 
    member(CN, CNs).

% `Need` is satisfied by an `FR` if the `FR` satisfies a list of needs `CNs`
% and `Need` is a member of the list `CNs`.
satisfied_by(Need, FR) :-
    ad(FR, _, CNs),
    member(Need, CNs).
