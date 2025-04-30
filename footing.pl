read_input(_, Input) :- 
    Input = "This is the data to use.".

design(_, Model) :-
    Model = designed_model.

report(_, Report) :-
    Report = "This is your report.".

main :-
    read_input("filename", Input),
    design(Input, Model),
    report(Model, Report),
    write(Report), nl, nl.
