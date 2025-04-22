student(joe).
designs_for(engineer, car, customer).
meets(design, code_requirements).
governs_design_of(aci_code, concrete_structure).
transitive_verb(subject, object).
verb(actor, object).
walks(owner, dog).

female(julie).
male(joe).
male(tommy).
male(joey).
person(X) :- male(X).
person(X) :- female(X).
mammal(X) :- person(X).
legs(X, 2) :- person(X).

% ?- male(joe).
% true.

% ?- male(julie).
% false.

% ?- person(joe).
% true .

% ?- person(julie).
% true.

% ?- mammal(joe).
% true .

% ?- mammal(X).
% X = joe .

% ?- mammal(julie).
% true.

% ?- person(joey).
% true .

% ?- person(P).
% P = joe ;
% P = tommy ;
% P = joey ;
% P = julie.

% ?- mammal(P).
% P = joe ;
% P = tommy ;
% P = joey ;
% P = julie.
