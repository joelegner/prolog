% buddhism.pl
% Key to variable names:
% P = person, being

% The First Noble Truth:
dukkha.

% The Second Noble Truth:
experiences(P, dukkha) :- experiences(P, clinging).

% The Third Noble Truth:
stops(P, dukkha) :- stops(P, clinging).

% The Fourth Noble Truth:
stops(P, clinging) :- follows(P, eightfold_noble_path).

% Mindfulness
mindful(Person) :-
    P = Person, % New thign I tried to improve readability
    aware(P, present_moment),
    balancing(P, the_mind),
    guarding(P, six_sense_doors),
    remembering(P, [buddha, dharma, sangha]).

main :-
    writeln('buddhism.pl').