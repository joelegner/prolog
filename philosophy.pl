% Good is a quality independent of conditions.
goodness.

% Identify things as having goodness or not
has(health, goodness).
has(love, goodness).
has(care, goodness).
has(caring, goodness).
has(concern, goodness).
has(pleasure, goodness).

does_not_have(pain, goodness).
does_not_have(anxiety, goodness).
does_not_have(addiction, goodness).
does_not_have(harm, goodness).
does_not_have(hate, goodness).

is_a(self_harm, harm).

all_good(Goods) :-
     findall(Good, has(Good, goodness), Goods).
