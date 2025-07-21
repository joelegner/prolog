% lists.pl
% Handling lists is an important thing in Prolog.
:- dynamic design_structure/2.

% Declare a list
my_list([apple, banana, cherry]).

% Get the first item in a list
list_first([First|_], First).

% Get the last item in the list by reversing the list and using first_item.
list_last(List, Last) :-
    reverse(List, Rev),
    list_first(Rev, Last).

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

% Double a list
% This is textbook recursion over lists using the cons constructor. 
% H = head, T = tail
% H2 = doubled list head, T2 = doubled list tail
double_list([], []).
double_list([H | T], [H2 | T2]) :-
    H2 is H * 2,
    double_list(T, T2).

/*
# Analysis of `double_list/2`

The pattern is this:

predicate([Head|Tail],...) :- 
    %Do something with Head,%
    predicate(Tail,...).

In Prolog literature, this pattern is generally referred to as a 'recursive list transformation' or 'recursive list processing' pattern.

More specifically, this example fits the common _map_ pattern, where a function is recursively applied to each element of a list to produce a new list. Although Prolog doesn't have higher-order functions like map/2 in functional languages, this recursive structure serves the same purpose:

1. The base case handles the empty list: `double_list([], [])`
2. The recursive case applies a transformation (H2 is H * 2) and recurses on the tail: `double_list([H|T], [H2|T2])`.

# Common Terminology in Prolog Contexts
- Recursive list predicate
- List mapping
- Element-wise transformation
- Structural recursion over lists
*/

% TEST CODE FOR LISTS
:- begin_tests(list_utils).

test(list_first) :-
    my_list(L),
    list_first(L, F),
    assertion(F == apple).

test(list_last) :-
    my_list(List),
    list_last(List, Last),
    assertion(Last == cherry).

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

% This is the first hand-coded test I ever wrote on May 13, 2025 in Valirco, Florida.
test(double_list) :- 
    double_list([5, 4, 3, 2, 1], Doubled),
    assertion(Doubled == [10, 8, 6, 4, 2]).

test(add_to_list_bratko) :-
    X = a,
    L1 = [b,c,d],
    % The big moment:
    L2 = [X|L1],
    assertion(L2 == [a,b,c,d]).

test(add_to_list_bratko_two) :-
    X1 = i,
    X2 = j,
    L1 = [k,l,m],
    % The big moment:
    L2 = [X1,X2|L1],
    assertion(L2 == [i, j, k, l, m]).

:- end_tests(list_utils).
