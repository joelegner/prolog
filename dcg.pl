% dcg.pl
% Definite clause grammars

/*
This file is based upon a viewing and study of this tutorial
on June 8, 2025:

https://www.youtube.com/watch?v=CvLsVfq6cks

Prolog was *designed* for text processing. It's heritage includes systems that translated weather reports between French and English.

Prolog string = List of characters.

Strings are easy to read and write.

Partially instantiated = Having unknown elements (does he mean variables?)

To work with strings we want the Prolog flag set:
double_quotes set to chars

At the top of your SWI Prolog file put this:
:- set_prolog_flag(double_quotes, chars).

?- append('hello', Ls1, 'hello world and universe!').

Here the Greeting is said to be everything up to the first space. 

?- append(Greeting, [' '|Ls1], 'hello world and universe!').
Greeting = 'hello', Ls1 = 'world and universe...'.

Executable specifications that work in all user modes.

GRHead means "grammar rule head."

Grammar rule example:

```prolog
GRHead --> GRBody.
```

A grammar rule defines a nonterminal.

What is a nonterminal?

A nonterminal is that which describes a sequence. 

A nonterminal stands for the elements it describes.

The rules of the same nonterminal are logical alternatives. 

Nonterminal indicator: F//N refers to the nonterminal F with N arguments. Note the double slash '//'. It is not a predicate indicator.

Predefined nonterminals include:

(',')//2 % concatenation, read as "and then"
('|')//2 % alternatives, read as "or"

A GRBody consists of:

- terminals,
- nonterminals, and
- grammar body goals.

Think of a terminal as a Prolog list. 

% This seems to be the key relationship to using DCG:
phrase(GRBody, Ls) :- % Ls is a list described by GRBody.

Sure, Ls is a list, but we are assuming that we are treating a
string as a list of characters, so we can use double quotes now
like these two grammar rules defining the nonterminal as//0:
*/

as --> "".
as --> "a", as.
/* 

What are the nonterminals in the code above?
(',')//2
as//0

What are the terminals?
[]
[a] % expressed as "a"

We can read the code informally:

1. The empty sequence is an acceptable sequence.
2. So is a sequence with first element 'a' and then as//0.

+------------------------------------------+
|  The entry point is phrase/2 predicate.  |
+------------------------------------------+

Phrase is cool because it can be used in different _modes_ which
you can think of as different _directions_. 

The most general query looks like this:

?- phrase(as, Ls).
Ls = [] ;
Ls = [a] ;
Ls = [a, a] ;
Ls = [a, a, a] ;

It asks "Are there any solutions at all?"

We can fill in missing variables with valid terminals:

?- phrase(as, [a, a, X, a, Y, a]).
X = Y, Y = a

When we write terminals with variables, we must use list sequence.
However, when we write _concrete_ terminals, we can use the double
quotes syntactic sugar:

We can test a concrete list for a solution:
?- phrase(as, [a,a,b,a,a,a]).
false.

?- phrase(as, "aabaaa").
false.

?- phrase(as, "aaaaaa").
true.
*/

bits --> "".
bits --> [1], bits.
bits --> [0], bits.

% Define the grammar
program --> [].
program --> command, program.

command --> keyword, identifier, [';'].

keyword --> [print] | [call].

identifier --> [joe].
identifier --> [legner].

/* 
We will use iterative deepening to get all possible outcomes.
Without iterative deepening, we may not get all possible outcomes.

?- length(P, _), phrase(program, P).
P = [] ;
P = [print, joe, ;] ;
P = [print, legner, ;] ;
P = [call, joe, ;] ;
P = [call, legner, ;] ;
P = [print, joe, ;, print, joe, ;] ;
P = [print, joe, ;, print, legner, ;] ;
P = [print, joe, ;, call, joe, ;] ;
P = [print, joe, ;, call, legner, ;] 

Here is use of the pipe character | meaning "or".
*/
xy --> [].
xy --> ( "X" | "Y" ), xy.

/*
It works like this, for one example:

?- phrase(xy, "XYXXYYXYXYXY").
true ;

Now we will look at **iterative deepening**. If we try this:
?- phrase(xy, Phrase).
Phrase = [] ;
Phrase = ['X'] ;
Phrase = ['X', 'X'] ;
Phrase = ['X', 'X', 'X'] 

This is called unfair enumeration. To make it fair, prefix the phrase call with a call to length.

?- length(Ls, _), phrase(xy, Ls).
Ls = [] ;
Ls = ['X'] ;
Ls = ['Y'] ;
Ls = ['X', 'X'] ;
Ls = ['X', 'Y'] ;

We can ask for length 3:
?- length(Ls, 3), phrase(xy, Ls).
Ls = ['X', 'X', 'X'] ;
Ls = ['X', 'X', 'Y'] ;
Ls = ['X', 'Y', 'X'] ;
Ls = ['X', 'Y', 'Y'] ;
Ls = ['Y', 'X', 'X'] ;
Ls = ['Y', 'X', 'Y'] ;
Ls = ['Y', 'Y', 'X'] ;
Ls = ['Y', 'Y', 'Y'] ;

# `seq//1`

Building block: seq//1 describing the list in its argument.

Another is qes//1 which is backwards sequence.
*/

qes([]) --> [].
qes([L|Ls]) --> qes(Ls), [L].

palindrome(Ls) :- phrase(qes(Ls), Ls).

/* 
Now we can test it:
?- palindrome("racecar").
true.

We can even complete a partial list:
palindrome([x,y,z|Ls]).
Ls = [y, x] ;
Ls = [z, y, x] ;
Ls = [_1888, z, y, x] ;
Ls = [_1888, _1888, z, y, x] 

Not sure what to make of the weird results like _1888.

This is an important building block: ...//0
*/

... --> [] | [_], ... .

/*
Define ... as either an empty list or any terminal
followed by dot-dot-dot (...). 

What can it be used for?

Get the last element of a list.
?- phrase((..., [Last]), "abc").
Last = c ;

We can detect if a substring occurs in a string.
?- phrase((..., "b", ...), "abcba").
true ;
true ;
false.

We can also generate lists that contain a substring. We will use our length/2 predicate to make a fair enumeration using **iterative deepening**. 

?- length(Ls, _), phrase((..., "b", ...), Ls).
Ls = [b] ;
Ls = [b, _] ;
Ls = [_, b] ;
Ls = [b, _, _] ;
Ls = [_, b, _] ;
Ls = [_, _, b] 

The next example finds any pairs of elements.
?- phrase((..., [X,X], ...), "helloo!!").
X = l ;
X = o ;
X = ! ;
false.

What about parsing?

Parsing is also called _syntactic analysis_ or _syntax analysis_.

Parsing comes from the Latin word _pars_, _paris_ f.: "part"

I thought Prolog was all about relations. If so, how can parsing be expressed as a relation? 

Parsing is expressed as a relation between (1) syntax trees and (2) lists of characters (or tokens). 

That is to say parsing is a relation between trees and lists.

Parsing is a specific usage mode of DCGs. It is the case where the sequence of characters is known and the tree is needed.

Let's parse expressions of the form "1", "1+1", "1+1+1", and so on.

NO! WAIT! Let's not _parse_ them. Let's _describe_ them. That's what we do in Prolog. We describe relations.

Let's _describe_ expressions of the form "1", "1+1", "1+1+1", and so on.
*/

% expr --> "1", expr_remainder.
% expr_remainder --> [].
% expr_remainder --> "+", expr.

/*
Let's try parsing. Parsing is relating lists of characters to syntax trees.  
*/
expr(1, [_|Rs], Rs) --> "1".
expr(A+B, [_|Rs0], Rs) --> expr(A, Rs0, Rs1), "+", expr(B, Rs1, Rs).
 
/*
To test this code:

?- Cs = "1+1+1", phrase(expr(E, Cs, _), Cs).
Cs = ['1', +, '1', +, '1'],
E = 1+(1+1) ;
Cs = ['1', +, '1', +, '1'],
E = 1+1+1 ;
false.
*/