% business.pl
synonym(deliverable, work_product).

is_synonym(X, Y) :- synonym(X, Y).
is_synonym(X, Y) :- synonym(Y, X).

synonyms(SynList) :-
     findall(Syn, is_synonym(Syn, _), SynList).

aspects(self, [emotional, ethical, financial, legal, physical, social]).
