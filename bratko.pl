%% bratko.pl

:- use_module(conc).

% Ivan Bratko, Prolog Programming for Artificial Intelligence (2012).

% Section 2.2, page 43

% vertical(seg(point(X1, Y1), point(X1, Y2))).
% horizontal(seg(point(X1, Y1), point(X2, Y1))).

% Exercise 2.6, page 46.

f(1, one).
f(s(1), two).
f(s(s(1)), three).
f(s(s(s(X))), N) :-
    f(X, N).

/*
The last clause is the most interesting. It looks better like this:

f(s(s(s(X))), N) :- f(X, N).

Now for this to be true, both sides need to be unified.

s(s(s(X))) = s(s(s(s(s(s(1)))))).

The X matches the inner part of the deeply nested structure. So we get X = s(s(s(1))).

Now our goal has become this:

f(s(s(s(1))), N).

But looky here! The new goal matches the recursive goal again. We need to unify the new call.

?- f(s(s(s(X))), N) = f(s(s(s(1))), N).
X = 1.

The right hand side now calls f(X, N) with X = 1. 

f(1, N).

Go back to your database above. See the match with f(1, one)? This is how we get N = one. Q.E.D.
*/

% Exercise 2.8, page 46.

/*
Rewrite the following program without using the semicolon notation.

Here is the original. The rewrite will be live code.

translate( Number, Word) :-
    Number = 1, Word = one;
    Number = 2, Word = two;
    Number = 3, Word = three.

?- translate(Number, Word).
Number = 1,
Word = one ;
Number = 2,
Word = two ;
Number = 3,
Word = three.

Here is the rewrite which follows coding standard recommendations. 
*/
translate( Number, Word) :-
    Number = 1, 
    Word = one.

translate( Number, Word) :-
    Number = 2, 
    Word = two.

translate( Number, Word) :-
    Number = 3, 
    Word = three.

/* 
Running my live code gives the same answer. Boom.

?- translate(Number, Word).
Number = 1,
Word = one ;
Number = 2,
Word = two ;
Number = 3,
Word = three.
*/

/* 
Sis a sublist of L if:
(1) L can be decomposed into two lists, Ll and L2, and
(2) L2 can be decomposed into two lists, S and some L3.
*/

sublist(S, L) :-
    conc(_, L2, L), 
    conc(S, _, L2).

del([X, [X|Tail]], Tail).

del(X, [Y|Tail], [Y|Tail1]) :-
    del(X, Tail, Tail1).

insert(X, List, BiggerList) :-
    del(X, BiggerList, List).

permutation([], []).
permutation(List, [First|Perm]) :-
    select(First, List, Rest),
    permutation(Rest, Perm).

unique_permutation(List, Perm) :-
    setof(P, permutation(List, P), Perms),
    member(Perm, Perms).

/*
?- findall(P, unique_permutation([a,a,a,a,a], P), Perms), length(Perms, PermCount).
Perms = [[a, a, a, a, a]],
PermCount = 1.

?- findall(P, unique_permutation([a,b,c,a,d], P), Perms), length(Perms, PermCount).
Perms = [[a, a, b, c, d], [a, a, b, d, c], [a, a, c, b, d], [a, a, c, d, b], [a, a, d, b|...], [a, a, d|...], [a, b|...], [a|...], [...|...]|...],
PermCount = 60.

?- findall(P, unique_permutation([a,b,c,e,d], P), Perms), length(Perms, PermCount).
Perms = [[a, b, c, d, e], [a, b, c, e, d], [a, b, d, c, e], [a, b, d, e, c], [a, b, e, c|...], [a, b, e|...], [a, c|...], [a|...], [...|...]|...],
PermCount = 120.

?- findall(P, unique_permutation([a,e,a,e,a], P), Perms), length(Perms, PermCount).
Perms = [[a, a, a, e, e], [a, a, e, a, e], [a, a, e, e, a], [a, e, a, a, e], [a, e, a, e|...], [a, e, e|...], [e, a|...], [e|...], [...|...]|...],
PermCount = 10.
*/