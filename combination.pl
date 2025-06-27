%% combination.pl

meat(beef).
meat(chicken).
meat(fish).
meat(pork).
meat(lamb).
wine(red).
wine(white).
wine(rose).
wine(blush).

combinations(Cs) :-
    findall(C, (meat(M), wine(W), C = [M, W]), Cs).
