female(julie).
male(joe).
male(tommy).
male(joey).
person(X) :- male(X).
person(X) :- female(X).
mammal(X) :- person(X).

parent(joe, joey).
parent(julie, joey).
parent(joe, tommy).
parent(julie, tommy).

married(joe, julie).
are_married(X, Y) :-
    married(X, Y), !; % note the cut operator `!`
    married(Y, X).

best_friend(joe, julie).
are_best_friends(X, Y) :-
    best_friend(X, Y), !; % note the cut operator `!`
    best_friend(Y, X).

% BEGIN TESTS ================================================
:- begin_tests(marriage).

test('Joe is married to Julie') :-
    are_married(joe, julie).

test('Julie is married to Joe') :-
    are_married(julie, joe).

:- end_tests(marriage).
% END TESTS ================================================
