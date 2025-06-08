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
*/
