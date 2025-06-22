%% reverse.pl
/*
As an exercise, I want to reverse a list like this:

?- reverse([a, b, c, d, e], Reverse).
Reverse = [e, d, c, b, a].

Let's try turning this into a predicate.

reverse([a, b, c, d, e], Reverse) :-
    Reverse = [e, d, c, b, a].

Tested. It works. Interesting. Here is the run:

?- reverse([H | T], Reverse).
reverse(T, TR),
Reverse = [TR | H].
*/

reverse([], []).
reverse([H | T], Reverse) :-
    reverse(T, TR),
    Reverse = [TR | H].

/*
It works! I think I did it. Here is a test:

?- reverse([a, b, c, d, e, f], Reverse).
Reverse = [f, e, d, c, b, a].

The breakthrouth was really realizing that the tail had to be reversed befor appending the head to it. That led me to add reverse(T, TR). The whole thing feels like magic. It feels like this must only be the interface to a much more lenghly and complicated program somehow behind the scenes. But it is not like that. It's this simple. With just some simple rules seemingly anything is possible.
*/

