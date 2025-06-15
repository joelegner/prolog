:- module(user_interface, [user_interface//0]).

user_interface -->
    first_use_view,
    normal_main_view,
    navigation_views,
    editor_views.

first_use_view --> onboarding_steps, first_time_storage_setup.
onboarding_steps --> [].
first_time_storage_setup --> [].

normal_main_view --> tab_bar, now_playing_view, set_list_view.
tab_bar --> [].
now_playing_view --> [].
set_list_view --> [].

navigation_views --> song_detail_view, artist_detail_view.
song_detail_view --> [].
artist_detail_view --> [].

editor_views --> song_editor, tag_editor.
song_editor --> [].
tag_editor --> [].
