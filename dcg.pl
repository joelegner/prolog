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

keyword --> [print].

identifier --> [joe].
identifier --> [legner].
