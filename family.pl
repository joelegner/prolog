female(julie).
female(julie).
female(elaine).
female(margo).
female(cher).
female(laura).
female(eileen).
female(jennifer).
female(molly).
female(lorraine).
female(carol).

male(joe).
male(tommy).
male(joey).
male(bob).
male(terry).
male(steve).
male(scott).
male(ron).
male(butch).
male(paul).
male(tom).
male(bruce).
male(richard).

person(X) :- male(X).
person(X) :- female(X).

parent(joe, joey).
parent(julie, joey).
parent(joe, tommy).
parent(julie, tommy).
parent(terry, julie).
parent(terry, steve).
parent(terry, scott).
parent(margo, julie).
parent(margo, scott).
parent(margo, steve).
parent(bob, joe).
parent(bob, ron).
parent(bob, richard).
parent(elaine, joe).
parent(elaine, richard).
parent(elaine, ron).
parent(tom, eileen).
parent(tom, butch).
parent(lorraine, eileen).
parent(lorraine, butch).
parent(carol, paul).
parent(carol, laura).
parent(bruce, paul).
parent(bruce, laura).

father(F, C) :- parent(F, C), male(F).
mother(F, C) :- parent(F, C), female(F).
grandparent(GP, C) :- parent(GP, P), parent(P, C).
grandmother(GP, C) :- grandparent(GP, C), female(GP).
grandfather(GP, C) :- grandparent(GP, C), male(GP).
child(C, P) :- parent(P, C).

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

test('Joe is Tommy\'s father') :-
    father(joe, tommy).

:- end_tests(marriage).
% END TESTS ================================================
