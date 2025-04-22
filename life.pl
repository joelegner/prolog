joe.
julie.
joey.
tommy.

bob.
terry.
elaine.
margo.

aristotle.
brando.
cher.
dylan.

person(joe).
person(julie).
person(joey).
person(tommy).
person(cher).
person(bob).
person(terry).
person(elaine).
person(margo).

living(joe).
living(julie).
living(joey).
living(tommy).
living(cher).
living(dylan).
living(bob).
living(terry).

female(julie).
female(elaine).
female(margo).
female(cher).

male(joe).
male(joey).
male(tommy).
male(bob).
male(terry).
male(dylan).

father(joe, joey).
father(joe, tommy).
father(terry, julie).
father(bob, joe).
mother(julie, joey).
mother(julie, tommy).
mother(elaine, joe).
mother(margo, julie).

grandparent(GP, C) :- parent(GP, P), parent(P, C).
grandmother(GP, C) :- grandparent(GP, C), female(GP).
grandfather(GP, C) :- grandparent(GP, C), male(GP).

husband(joe, julie).
husband(terry, margo).
wife(julie, joe).
wife(margo, terry).
spouse(H, W) :- husband(H, W), wife(W, H).

% Relationship rules
parent(P, C) :- father(P, C); mother(P, C).
child(C, P) :- father(P, C); mother(P, C).
family(P1, P2) :- 
    parent(P1, P2);
    child(P1, P2);
    spouse(P1, P2).
loves(P1, P2) :- family(P1, P2).
loves(X, X).
loves(joe, aristotle).

% P is deceased if P is a person and P is not living.
% Another way to say it:
% For every P, P is a member of deceased if P is a 
% member of person and P is not a member of living.
% The symbol \+ is negation.
deceased(P) :- person(P), \+ living(P).
