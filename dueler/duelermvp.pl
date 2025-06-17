:- module(duelermvp, [run/0]).

:- use_module(app).
:- use_module(data_model).
:- use_module(user_interface).
:- use_module(helpers).
:- use_module(tests).

source_code_folder -->
    ['Sources/'],
    'DuelerMVPApp/',
    'Models/',
    'Views/',
    'ViewModels/',
    [].

'DuelerMVPApp/' -->
    ['DuelerMVPApp/'],
    'DuelerMVPApp.swift'.

'DuelerMVPApp.swift' -->
    ['DuelerMVPApp.swift'],
    'struct DuelerMVPApp'.

'struct DuelerMVPApp' --> 
    ['struct DuelerMVPApp'].

'Models/' --> 
    ['Models/'],
    ['Song.swift'],
    ['Playlist.swift'],
    [].

'Views/' --> 
    ['Views/'],
    'FirstTimeView',
    'MainView/',
    [].

'ViewModels/' -->
    ['ViewModels/'],
    [].

'FirstTimeView' -->
    ['FirstTimeView.swift'].

'MainView/' -->
    ['MainView/'],
    ['MainView.swift'],
    ['SetListSidebarView.swift'],
    ['SongFinderSidebarView.swift'],
    ['LyricsView.swift'],
    ['LyricsEditorView.swift'].

run :-
    phrase(source_code_folder, Repository),
    writeln(Repository).