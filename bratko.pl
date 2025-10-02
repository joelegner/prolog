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

/*
More reading on Wednesday, October 1, 2025.

We want a predicate `sumlist(List, PartialSum, TotalSum)` where TotalSum = PartialSum + sum over List.

Let's start by calling an AUXILIARY PREDICATE. This is a common pattern in Prolog. Here the original predicate is `sumlist/2` and the auxiliary predicate is `sumlist/3`. The auxiliary predicate lets us add an accumulator. In the code below, the accumulator is `PartialSum`.
*/
sumlist(List, Sum) :-
    sumlist(List, 0, Sum).

/*
The base case is an empty list. Since we are using this recursively, it is the terminating case. That's why we want it to come before the recursive case that follows it. You might thing the code should be:

```prolog
sumlist([], 0, 0). 
```

The problem with this is that when we use it recursively, it will report zero for the final answer. Instead what we want is to assign the value PartialSum to the value TotalSum. We could do it like this to be verbose:

```
sumlist([], PartialSum, TotalSum) :-
    PartialSum = TotalSum.
```

But this can be refactored. This is another common Prolog pattern:

Refactor `f(X, Y) :- X = Y` into `f(X, X).`. This eliminates one variable and puts the same variable twice in the head. That way the two instances of `X` are unified by Prolog when the program runs. 

So here is our resulting predicate:
*/
sumlist([], Sum, Sum).

/*
We now need to do the work with the recursive predicate. We have our accumulator, `PartialSum`. The list gets split into its head `First` and the `Rest` of the list. First is an element of the list. Rest is a list, which could include the empty list `[]`.

We have a before `PartialSum` and an after `NewPartialSum`. The last line is the recursive call. As you can see, it passes the `Rest` of the list and the `NewPartialSum` back into itself. Once we get to the last element in the list, `Rest=[]` and the above predicate `sumlist([], Sum, Sum)` does the equivalent of setting TotalSum equal to NewPartialSum. 
*/
sumlist([First | Rest], PartialSum, TotalSum) :-
    NewPartialSum is PartialSum + First,
    sumlist( Rest, NewPartialSum, TotalSum).


/*
# Example Usage of sumlist/2:

Example Execution of `sumlist([1,2,3], Sum).`

1. Call: sumlist([1,2,3], Sum)
2. Expand: sumlist([1,2,3], 0, Sum)

3. Match recursive clause:
   PartialSum = 0, First = 1, Rest = [2,3]
   NewPartialSum = 0 + 1 = 1
   Next call: sumlist([2,3], 1, Sum)

4. Match recursive clause:
   PartialSum = 1, First = 2, Rest = [3]
   NewPartialSum = 1 + 2 = 3
   Next call: sumlist([3], 3, Sum)

5. Match recursive clause:
   PartialSum = 3, First = 3, Rest = []
   NewPartialSum = 3 + 3 = 6
   Next call: sumlist([], 6, Sum)

6. Match base case:
   sumlist([], Sum, Sum)
   Unify: Sum = 6

Final Result: Sum = 6
*/

/*
# Trace Session

The `leash(-all)` call makes it possible to run the trace without typing `c` for `creep` every time. 

?- leash(-all), trace, sumlist([1,2,3], Sum). 
   Call: (13) sumlist([1, 2, 3], _112256)
   Call: (14) sumlist([1, 2, 3], 0, _112256)
   Call: (15) _116234 is 0+1
   Exit: (15) 1 is 0+1
   Call: (15) sumlist([2, 3], 1, _112256)
   Call: (16) _118676 is 1+2
   Exit: (16) 3 is 1+2
   Call: (16) sumlist([3], 3, _112256)
   Call: (17) _121118 is 3+3
   Exit: (17) 6 is 3+3
   Call: (17) sumlist([], 6, _112256)
   Exit: (17) sumlist([], 6, 6)
   Exit: (16) sumlist([3], 3, 6)
   Exit: (15) sumlist([2, 3], 1, 6)
   Exit: (14) sumlist([1, 2, 3], 0, 6)
   Exit: (13) sumlist([1, 2, 3], 6)
Sum = 6.
*/