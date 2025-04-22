joe.
julie.
joey.
tommy.

aristotle.
brando.
cher.
dylan.

person(joe).
person(julie).
person(joey).
person(tommy).
person(cher).

living(joe).
living(julie).
living(joey).
living(tommy).
living(cher).
living(dylan).

father(joe, joey).
father(joe, tommy).
mother(julie, joey).
mother(julie, tommy).
male(joe).
male(joey).
male(tommy).
female(julie).
female(cher).

husband(joe, julie).
wife(julie, joe).
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
