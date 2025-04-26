% General morphological analysis

% Terms
% Morphological Field - Set of variables + possible values

has_variable(field, variable1).
has_variable(field, variable2).
has_variable(field, variable3).
has_variable(field, variable4).

has_values(variable1, [value1a, value1b, value1c]).
has_values(variable2, [value2a, value2b, value2c]).
has_values(variable3, [value3a, value3b, value3c]).
has_values(variable4, [value4a, value4b, value4c]).

print_list(List) :-
    forall(member(M, List),
    (
        write("    "),
        writeln(M))
    ).

print_variable(V) :-
    has_variable(_, V),
    forall(has_values(V, Values),
    (print_list(Values))
    ).

print_variables :-
    forall(has_variable(_, Variable), 
    ( writeln(Variable), print_variable(Variable))
    ).

% TODO: Add configureation which is a set of values
% TODO: Some variables can have more than one selection
% TODO: When value a is selected, these other values become unavailable

main :-
    writeln("General Morphological Analysis"),
    print_variables.