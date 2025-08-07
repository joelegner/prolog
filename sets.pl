%% sets.pl
/*
Let's play around with sets. There is this predicate I know:

list_to_set(+List, ?Set)

https://www.swi-prolog.org/pldoc/man?predicate=list_to_set/2

On SWI Prolog, this seems to work out of the gate without needing to use the lists module.

?- list_to_set([3,1,2,3,1,4],L).
L = [3,1,2,4].

It appears to have eliminated the second 3 and the second 1 to achieve no duplicates. It did not change the order. 

You can put a variable in the ?Set:

?- list_to_set([3,1,2,3,1,4],[3,1,2,X]).
X = 4.

?- list_to_set([3,1,2,3,1,Y],[3,1,2,4]).
Y = 4.

But what's going on here?

?- list_to_set([Y,1,2,3,1,4],[3,1,2,4]).
false.

It fails. Not sure why. I expected Y = 3. Maybe it is because Y is not instantiated early enough. If I assign Y myself, it works, but this is hardly what I want. 

?- Y = 3, list_to_set([Y,1,2,3,1,4],[3,1,2,4]).
Y = 3.
*/