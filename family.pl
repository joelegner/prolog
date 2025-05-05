female(carol).
female(cher).
female(eileen).
female(elaine).
female(jennifer).
female(julie).
female(laura).
female(lorraine).
female(margo).
female(molly).
female(cecelia).
female(ruth).

male(bob).
male(bruce).
male(butch).
male(joe).
male(joey).
male(paul).
male(richard).
male(ron).
male(scott).
male(steve).
male(terry).
male(tom).
male(tommy).
male(meritt).
male(francis).

parent(bob, joe).
parent(bob, richard).
parent(bob, ron).
parent(bruce, laura).
parent(bruce, paul).
parent(carol, laura).
parent(carol, paul).
parent(cecelia, bob).
parent(elaine, joe).
parent(elaine, richard).
parent(elaine, ron).
parent(francis, bob).
parent(joe, joey).
parent(joe, tommy).
parent(julie, joey).
parent(julie, tommy).
parent(lorraine, butch).
parent(lorraine, eileen).
parent(margo, julie).
parent(margo, scott).
parent(margo, steve).
parent(meritt, carol).
parent(meritt, elaine).
parent(meritt, lorraine).
parent(ruth, carol).
parent(ruth, elaine).
parent(ruth, lorraine).
parent(terry, julie).
parent(terry, scott).
parent(terry, steve).
parent(tom, butch).
parent(tom, eileen).

father(F, C) :- 
    male(F),
    parent(F, C). 

mother(F, C) :- 
    female(F),
    parent(F, C).

grandparent(GP, C) :- 
    parent(GP, P), 
    parent(P, C).

grandmother(GP, C) :- 
    female(GP),
    grandparent(GP, C).

grandfather(GP, C) :- 
    male(GP),
    grandparent(GP, C).

% X = child 1
% Y = child 2
% M = mother
% F = father
sibling(X,Y) :- 
    mother(M,X),
    father(F,X),
    mother(M,Y),
    father(F,Y),
    X \= Y.

% C1 = cousin 1
% C2 = cousin 2
% AU1 = aunt or uncle 1, parent of cousin 1
% AU2 = aunt or uncle 2, parent of cousin 2
cousin(C1, C2) :-
    parent(AU1, C1),
    parent(AU2, C2),
    sibling(AU1, AU2),
    C1 \= C2.

child(C, P) :- father(P, C).
child(C, P) :- mother(P, C).

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
