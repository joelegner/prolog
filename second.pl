%% second.pl
/*
Let's try to return the second element.

?- second([a, b, c, d, e], Second).
Second = b.

We will start again by making this immediately a predicate.

second(a | Second | [c, d, e], Second) :-
    Second = b.
*/

second([_,X|_], X).

/*
Sample run:

?- second([a,b,c,d,e], X).
X = b.

It works in both directions, too:

?- second([a,X,c,d,e], b).
X = b.

Success again. This time the insight was remembering that you could include more than one variable in the head side of the [H|T] structure. You can do [H1,H2|T] for example. If we then ignore the other two variables in the relation, we keep the second term only. [_, H2|T]. Rename H2 to X to make it generic and vois la. We have second([_,X|_], X).
*/
