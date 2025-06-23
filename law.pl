% law.pl
/*
The law (the legal system) can help us understand Prolog rules.

mandatory(action)     :- condition1, condition2, ..., condition_n.
prohibitory(action)   :- condition1, condition2, ..., condition_n.
discretionary(action) :- condition1, condition2, ..., condition_n.
declaratory(term)     :- condition1, condition2, ..., condition_n.

Or more generally:
functor(term)         :- condition1, condition2, ..., condition_n.

Head :- Body

Head is the result of the law.
Body is the "test"--the set of conditions that triggers the response. The body is a conjunction of terms. It is a GOAL in Prolog parlance. 

       Result                            Test
~~~~~~~~~~~~~~~~~~~~~    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
discretionary(action) :- condition1, condition2, ..., condition_n.

*/

principles(law, [principle(live_honestly), principle(hurt_nobody), principle(give_everybody_their_due)]).

% Extract all principles from the list
principle(P) :-
    principles(_, Principles),
    member(principle(P), Principles).

% Definition of robbery using theft and force
defined_as(robbery, theft, [force]).

% Gather all principles into a list
all_principles(Ps) :-
    findall(P, principle(P), Ps).