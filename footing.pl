round_up_to_half(X, Y) :-
    Y is ceil(X * 2) / 2.

round_up_to_quarter(X, Y) :-
    Y is ceil(X * 4) / 4.

bearing_area_required(LoadKips, SoilCapacityPsf, AreaSqFt) :-
    AreaSqFt is LoadKips*1000.0/SoilCapacityPsf.

design_footing(P, Q, Brounded) :-
    bearing_area_required(P, Q, Areq),
    B is sqrt(Areq),
    round_up_to_quarter(B, Brounded).

display_feet_inches(FeetDecimal) :-
    Feet is floor(FeetDecimal),
    Fraction is FeetDecimal - Feet,
    InchesFloat is Fraction * 12,
    Inches is round(InchesFloat),
    format("~w'-~w\"~n", [Feet, Inches]).

main :-
    writeln('Design a spread footing.'), nl,
    P = 25,
    Q = 1500.0,
    design_footing(P, Q, Breq),
    write('Required footing dimension (rounded up to nearest 0.5 ft): '),
    write(Breq), write(' ft'), nl,
    write('Which is: '),
    display_feet_inches(Breq).
