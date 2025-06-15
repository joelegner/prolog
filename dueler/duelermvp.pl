% duelermvp.pl
% This DCG defines the structure of the DuelerMVP Swift source code.

source_code --> 
    app_source_code,
    test_source_code.

% App code consists of the app entry point, data, UI, and helpers
app_source_code --> 
    the_app,
    data_model,
    user_interface,
    helpers.

% The main app struct and entry point
the_app --> 
    app_entry_point,
    app_lifecycle,
    app_navigation.

app_entry_point --> 
    [].

app_lifecycle --> 
    [].

app_navigation --> 
    [].

% The data model includes all app data types
data_model --> 
    song_data_model,
    tag_data_model,
    set_list_data_model,
    artist_data_model,
    user_data_model.

song_data_model --> 
    song_entity,
    song_fields,
    song_relationships.

song_entity --> 
    [].

song_fields --> 
    [].

song_relationships --> 
    [].

tag_data_model --> 
    tag_entity,
    tag_fields.

tag_entity --> 
    [].

tag_fields --> 
    [].

set_list_data_model --> 
    set_list_entity,
    set_list_fields,
    set_list_relationships.

set_list_entity --> 
    [].

set_list_fields --> 
    [].

set_list_relationships --> 
    [].

artist_data_model --> 
    artist_entity,
    artist_fields.

artist_entity --> 
    [].

artist_fields --> 
    [].

user_data_model -->
    user_entity,
    user_fields,
    user_preferences.

user_entity --> 
    [].

user_fields --> 
    [].

user_preferences --> 
    [].

% The user interface
user_interface --> 
    first_use_view,
    normal_main_view,
    navigation_views,
    editor_views.

first_use_view --> 
    onboarding_steps,
    first_time_storage_setup.

onboarding_steps --> 
    [].

first_time_storage_setup --> 
    [].

normal_main_view --> 
    tab_bar,
    now_playing_view,
    set_list_view.

tab_bar --> 
    [].

now_playing_view --> 
    [].

set_list_view --> 
    [].

navigation_views --> 
    song_detail_view,
    artist_detail_view.

song_detail_view --> 
    [].

artist_detail_view --> 
    [].

editor_views --> 
    song_editor,
    tag_editor.

song_editor --> 
    [].

tag_editor --> 
    [].

% Reusable utilities
helpers --> 
    date_helpers,
    sorting_helpers,
    formatting_helpers.

date_helpers --> 
    [].

sorting_helpers --> 
    [].

formatting_helpers --> 
    [].

% Tests and dev tools
test_source_code --> 
    unit_tests,
    integration_tests.

unit_tests --> 
    song_model_tests,
    set_list_tests.

song_model_tests --> 
    [].

set_list_tests --> 
    [].

integration_tests --> 
    first_use_flow_test,
    set_list_editing_test.

first_use_flow_test --> 
    [].

set_list_editing_test --> 
    [].
