% colors.pl
thing(apple, red, yes).
thing(broccoli, green, yes).
thing(carrot, orange, yes).
thing(dirt, brown, no).
thing(grass, green, no).
thing(lettuce, green, yes).
thing(rice, white, yes).
thing(sky, blue, no).

% Practice problems 
% https://www2.cs.arizona.edu/classes/cs372/fall06/prolog.sli.pdf


% What is green that is not a food?
green_not_food(GNF) :- 
    thing(GNF, green, F),
    dif(F, yes).

% ?- green_not_food(X).
% X = grass .
% Correct

% What color is lettuce?
color_of_lettuce(C) :-
    thing(lettuce, C, _).

% ?- color_of_lettuce(C).
% C = green.
% Correct

% What foods are orange?
orange_foods(F) :-
    thing(F, orange, yes).

% ?- orange_foods(F).
% F = carrot.
% Correct

% What foods are the same color as lettuce?
foods_same_color_as_lettuce(F) :-
    thing(lettuce, C, _),
    thing(F, C, yes).

% ?- foods_same_color_as_lettuce(F).
% F = broccoli ;
% F = lettuce.
% Correct