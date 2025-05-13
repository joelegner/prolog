% law.pl
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