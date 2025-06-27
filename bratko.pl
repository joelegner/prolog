%% bratko.pl

% Ivan Bratko, Prolog Programming for Artificial Intelligence (2012).

% Section 2.2, page 43

vertical(seg(point(X1, Y1), point(X1, Y2))).
horizontal(seg(point(X1, Y1), point(X2, Y1))).

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

Here is the rewrite:
*/
translate( Number, Word) :-
    Number = 1, Word = one.

translate( Number, Word) :-
    Number = 2, Word = two.

translate( Number, Word) :-
    Number = 3, Word = three.

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