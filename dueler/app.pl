:- module(app, [app_source_code//0, run/0]).

make_dueling_fun -->
    welcome_new_users,
    maintain_song_database,
    view_lyrics.

welcome_new_users -->
    ['struct NewUserView'].

view_lyrics -->
    find_songs_fast,
    display_lyrics.

find_songs_fast -->
    ['struct SongFinderView'].

display_lyrics -->
    ['struct LyricsView'].

maintain_song_database -->
    edit_song_data,
    edit_set_lists.

edit_song_data --> 
    edit_song_metadata,
    edit_song_lyrics.

edit_song_metadata -->
    ['struct SongMetadataView'],
    edit_song_title,
    edit_song_artist.

edit_song_lyrics -->
    ['struct LyricsEditor'].

edit_song_title -->
    ['Song Title Field'].

edit_song_artist -->
    ['Song Artist Field'].

edit_set_lists -->
    ['struct SetListEditorView'].

the_app --> 
    the_app_imports,
    nl,
    the_app_struct_declaration,
    ['{'], nl,
    the_app_struct_body,
    ['}'], nl.


app_source_code --> 
    the_app,
    data_model,
    user_interface,
    helpers.

the_app_struct_declaration -->
    ['@main'], nl,
    struct('DuelerMVPApp').

the_app_imports -->
    ['import SwiftUI'], nl.

the_app_struct_body -->
    [].

run :-
    phrase(make_dueling_fun, Report),
    writeln(Report).