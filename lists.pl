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

/*
Some things inspired by Ivan Bratko. First here is a list. 

Interestingly, this declares the four atoms. 
?- ann.
true.

?- annie.
ERROR: Unknown procedure: annie/0 (DWIM could not correct goal)

Here is the usual Head|tail stuff:

?- [Head | Tail] = [ann, tennis, tom, skiing].
Head = ann,
Tail = [tennis,tom,skiing].

?- [ann, tennis, tom, skiing] = [ann, T | _].
T = tennis.

.( Head, Tail )
*/

/*
Bratko Exercise 3.3:
Define two predicates: evenlength(List) and oddlength(List). They are true when the length of the list is even or odd, respectively.
*/

evenlength(List) :-
    length(List, Length),
    Mod is Length mod 2,
    Mod #= 0.

oddlength(List) :-
    length(List, Length),
    Mod is Length mod 2,
    Mod #= 1.

/*
Bratko Exercise 3.6. My solution works.

?- shift([1,2,3,4,5], L1), shift(L1, L2).
L1 = [2,3,4,5,1],
L2 = [3,4,5,1,2].
*/
shift(List, Shifted) :-
    List = [H|T],
    append(T, [H], Shifted).

/*
Bratko Exercise 3.8.
Define the relation subset(Set, Subset) where Set and Subset are two lists representing two sets. We would like to be able to use this relation not only to check for the subset relation, but also to generate a possible subsets of a given set.

This seems to work, but it feels like a kludge and not very elegant. It might just work by luck. It should be refactored or totally re-imagined.

But at any rate, it seems to generate subsets.

?- subset([a,b,c], S).
S = [a,b,c] ;
S = [b,c] ;
S = [a] ;
S = [b] ;
S = [c].
*/
subset(Set, Subset) :-
    append(_, Subset, Set),
    length(Subset, L),
    L #> 1.
subset(Set, Subset) :-
    member(X, Set),
    Subset = [X],
    length(Subset, L),
    L #> 0.

/*
Bratko Exercise 3.10
This one seems easy. Define the predicate equal_length(L1, L2) which is true if lists L1 and L2 have equal number of elements. It works.

?- equal_length([a,b,c,d], [1,2,[],4]).
true.

?- equal_length([a,b,c,d], [1,2,4]).
false.
*/
equal_length(L1, L2) :-
    length(L1, L),
    length(L2, L).

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

/* Tests for Bratko Exercise 3.3 */
test(even_length_list) :-
    evenlength([a, b, c, d]).

test(even_length_list_fail, [fail]) :-
    evenlength([a, b, c]).

test(odd_length_list) :-
    oddlength([a, b, c]).

test(odd_length_list_fail, [fail]) :-
    oddlength([a, b, c, d]).

/* Test for Bratko Exercise 3.6 */
test(shift) :-
    shift([1,2,3,4,5], L1),
    shift(L1, L2),
    L1 = [2,3,4,5,1],
    L2 = [3,4,5,1,2].

/* Test for Bratko Exercise 3.10 */
test(equal_length) :-
    equal_length([a,b,c,d], [1,2,[],4]).

test(equal_length_fail, [fail]) :-
    equal_length([a,b,c,d], [1,[],4]).

:- end_tests(list_utils).
