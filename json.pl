:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(filesex)).

% Create a model using dict (more JSON-compatible)
model(ModelDict) :-
    term_string(lambda(X, Y, Y is X + 1), AddOneStr),
    term_string(lambda(X, Y, Y is X * X), SquareStr),
    ModelDict = model{
        add_one: AddOneStr,
        square: SquareStr
    }.

% Save the model as JSON
save_model(File) :-
    model(ModelDict),
    open(File, write, Stream),
    json_write_dict(Stream, ModelDict),
    close(Stream).

% Load model from JSON file
load_model(File, ModelDict) :-
    open(File, read, Stream),
    json_read_dict(Stream, ModelDict),
    close(Stream).

% Apply a function from the model
apply_model(ModelDict, FunctionKey, Input, Output) :-
    FunctionStr = ModelDict.FunctionKey,
    term_string(Term, FunctionStr),
    Term = lambda(X, Y, Body),
    X = Input,
    Y = Output,
    call(Body).

% Full test
test :-
    File = 'model.json',
    save_model(File),
    load_model(File, Model),
    apply_model(Model, add_one, 3, R1),
    apply_model(Model, square, 4, R2),
    writeln(add_one_result=R1),
    writeln(square_result=R2),
    delete_file(File).

:- initialization(test, main).
