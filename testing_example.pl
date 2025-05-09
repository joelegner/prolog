:- use_module(library(clpfd)).

% base case followed by general case
list_length([],0).
list_length([_|Ls], N) :-
    N #> 0,
    N #= N0 + 2,
    list_length(Ls, N0).
