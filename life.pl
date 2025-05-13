% life.pl
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


husband(joe, julie).
husband(terry, margo).
wife(julie, joe).
wife(margo, terry).
spouse(H, W) :- husband(H, W), wife(W, H).

% Relationship rules
grandparent(GP, C) :- parent(GP, P), parent(P, C).
grandmother(GP, C) :- grandparent(GP, C), female(GP).
grandfather(GP, C) :- grandparent(GP, C), male(GP).
parent(P, C) :- father(P, C).
parent(P, C) :- mother(P, C).
child(C, P) :- father(P, C).
child(C, P) :- mother(P, C).

% P is deceased if P is a person and P is not living.
% Another way to say it:
% For every P, P is a member of deceased if P is a 
% member of person and P is not a member of living.
% The symbol \+ is negation.
deceased(P) :- person(P), \+ living(P).
