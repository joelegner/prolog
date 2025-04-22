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

main :-
    writeln("buddhism.pl").