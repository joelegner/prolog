%% nato_alphabet.pl

letter_phonetic(a, alpha).
letter_phonetic(b, beta).
letter_phonetic(c, charlie).
letter_phonetic(d, delta).
letter_phonetic(e, echo).
letter_phonetic(f, foxtrot).
letter_phonetic(g, golf).
letter_phonetic(h, hotel).
letter_phonetic(i, india).
letter_phonetic(j, juliet).
letter_phonetic(k, kilo).
letter_phonetic(l, lima).
letter_phonetic(m, mike).
letter_phonetic(n, november).
letter_phonetic(o, oscar).
letter_phonetic(p, papa).
letter_phonetic(q, quebec).
letter_phonetic(r, romeo).
letter_phonetic(s, sierra).
letter_phonetic(t, tango).
letter_phonetic(u, uniform).
letter_phonetic(v, victor).
letter_phonetic(w, whisky).
letter_phonetic(x, xray).
letter_phonetic(y, yankee).
letter_phonetic(z, zulu).

nato_alphabet_pairs(NAP) :-
    findall(L-P, letter_phonetic(L, P), NAP).

run :-
    nato_alphabet_pairs(NAP),
    % This is a cool way to print all of a list
    maplist(writeln, NAP).
