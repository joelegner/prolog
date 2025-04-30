% footing.pl

% This is a mockup of the read_input that will take a filename
% and return a Project. 
read_input(_, Project) :- 
    
    % I think this is my first ever prolog dictionary (Project mock):
    Project = _{
    name: "Test Project",
    design: _{
        name: "Footing 1",
        footing: [4, 4, 1],
        load: [20, 40, 60]
    }
}.

design(_, Model) :-
    Model = designed_model.

report(_, Report) :-
    Report = "This is your report.".

main :-
    read_input("filename", Project),
    design(Project, Model),
    report(Model, Report),
    write(Report), nl, nl.
