footing_area(B, A) :- 
    A is B*B.

design_footing(P, Q, B) :-
    B is ceil(4*sqrt(1000.0*P/Q))/4.0.

main :-
    writeln('Design a spread footing.'), 
    nl,
    P = 35,
    Q = 1500.0,
    design_footing(P, Q, B),
    write('Required footing dimension: '),
    write(B), write(' ft'), 
    nl.