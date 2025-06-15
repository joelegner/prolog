:- module(duelermvp, [run/0]).

:- use_module(app).
:- use_module(data_model).
:- use_module(user_interface).
:- use_module(helpers).
:- use_module(tests).

source_code --> 
    app_source_code,
    test_source_code.

run :-
    phrase(source_code, SourceCode),
    writeln(SourceCode).