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

atom_phonetic_list(Atom, Phonetics) :-
    atom_chars(Atom, Chars),
    maplist(downcase_char, Chars, LowerChars),
    atom_phonetic_list_(LowerChars, Phonetics).

atom_phonetic_list_([], []).
atom_phonetic_list_([A|As], [P|Ps]) :-
    atom_char(A),
    letter_phonetic(A, P),
    atom_phonetic_list_(As, Ps).

downcase_char(Char, Lower) :-
    char_type(Char, upper),
    char_type(Lower, to_lower(Char)).
downcase_char(Char, Char).

atom_char(C) :- char_type(C, alpha).

run :-
    nato_alphabet_pairs(NAP),
    maplist(writeln, NAP),
    atom_phonetic_list(github, Ps),
    writeln(Ps).