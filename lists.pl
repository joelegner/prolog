% lists.pl
% Handling lists is an important thing in Prolog.

% Declare a list
my_list([apple, banana, cherry]).

% Get the first item in a list
first_item([First|_], First).

% Get the rest of the list
rest_of_list([_|Rest], Rest).

% Iterate to print all items in a list
print_list([]).
print_list([H|T]) :-
    writeln(H),
    print_list(T).

% Append two lists
append_lists(List1, List2, Result) :-
    append(List1, List2, Result).

% Remove duplicates from a list
remove_duplicates([], []).
remove_duplicates([First|Rest], [First|New_Rest]) :-
    exclude(==(First), Rest, Filtered),
    remove_duplicates(Filtered, New_Rest).

% TEST CODE FOR LISTS
:- begin_tests(list_utils).

test(first_item) :-
    my_list(L),
    first_item(L, F),
    assertion(F == apple).

test(rest_of_list) :-
    my_list(L),
    rest_of_list(L, R),
    assertion(R == [banana, cherry]).

test(print_list, true) :-
    my_list(L),
    print_list(L).

test(append_lists) :-
    append_lists([1, 2], [3, 4], R),
    assertion(R == [1, 2, 3, 4]).

% Tests for remove_duplicates
test(remove_duplicates_no_dupes) :-
    remove_duplicates([a, b, c], R),
    assertion(R == [a, b, c]).

test(remove_duplicates_with_dupes) :-
    remove_duplicates([a, b, a, c, b], R),
    assertion(R == [a, b, c]). % Order of kept elements is reversed due to logic

test(remove_duplicates_all_same) :-
    remove_duplicates([x, x, x, x], R),
    assertion(R == [x]).

test(remove_duplicates_empty) :-
    remove_duplicates([], R),
    assertion(R == []).

:- end_tests(list_utils).
