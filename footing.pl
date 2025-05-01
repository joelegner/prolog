% footing.pl
:- use_module(library(http/json)).        % For json_write/3, json_read/2, etc.
:- use_module(library(http/json_convert)). % Optional: for term <-> JSON conversion

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

save_project(File, Project) :-
    open(File, write, Stream),
    writeq(Stream, Project),
    write(Stream, '.'),
    close(Stream).

load_project(File, Project) :-
    open(File, read, Stream),
    read(Stream, Project),
    close(Stream).


design(_, Model) :-
    Model = designed_model.

report(_, Report) :-
    Report = "This is your report.".

:- begin_tests(json_io).
:- use_module(library(http/json)).

test(write_and_read_json_roundtrip) :-
    File = 'test.json',
    Project = _{
        name: "Test Project",
        design: _{
            name: "Footing 1",
            footing: [4, 4, 1],
            load: [20, 40, 60]
        }
    },

    % Write JSON to file
    open(File, write, Out),
    json_write(Out, Project, [width(0)]),
    close(Out),

    % Read JSON back from file
    open(File, read, In),
    json_read_dict(In, ReadProject),
    close(In),

    % Assert equality
    assertion(ReadProject = Project).
:- end_tests(json_io).

