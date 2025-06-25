%% units.pl

feet_inches(F, I) :-
    (   ground(I) -> 
        must_be(number, I),
        F is I / 12
    ;   ground(F) ->
        must_be(number, F),
        I is F * 12
    ;   throw(error(instantiation_error, feet_inches/2))
    ).