:- module(app, [app_source_code//0, run/0]).

/*
This domain specific language for axiomatic design is envisioned this way.

fr1 -->
    [dp1],
    fr11,
    fr12,
    fr13.

fr11 -->
    [dp11],
    fr111,
    fr112.

fr12 -->
    [dp12].

fr13 -->
    [dp13].

fr111 -->
    [dp111].

fr112 -->
    [dp112].
*/

make_dueling_fun -->
    ['DuelerMVP App for iPad'],
    welcome_new_users,
    maintain_song_database,
    view_songs.

welcome_new_users -->
    ['struct NewUserView'],
    create_new_database,
    open_existing_database,
    import_set_list_maker_database.

create_new_database -->
    ['struct OpenDatabaseView'].

open_existing_database -->
    ['struct NewDatabaseView'].

import_set_list_maker_database -->
    ['struct ImportDatabaseView'].

view_songs -->
    ['struct SongView'],
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