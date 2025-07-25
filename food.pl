%% food.pl
material(food).

whole_parts(food, [protein, carbohydrate, fat]).

substance(food).
substance(hot_dog).

edible(food).

uncountable(food).

concept(food).

word(food).
word_part_of_speech(word(food), part_of_speech(noun)).

food(F) :-
    substance(F),
    eats(animal, F),
    nourishes(F, animal).

eats(animal, hot_dog).
nourishes(hot_dog, animal).

:- begin_tests(food_tests).

test(hot_dog) :-
    assert(food(hot_dog)).

:- end_tests(food_tests).
