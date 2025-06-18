#!/usr/bin/env swipl
% rando.py

% We are telling the intepreter to start with our main/1 predicate. 
:- initialization(main, main).

/*
This is the darndest thing. Look at this code. This is all the code it took in Prolog to implement my rando program that I originally wrote in Ada. For giggles, the Ada code is given below. Look at how arcane it looks. It is harder to read, harder to understand than the Prolog code. The Prolog code is marvelous. 
*/

/* 
The argument of main/1 is a list of args passed to the command line program. We first set up the base case: no arguments passed. For this case we use the special empty list atom `[]`. Here we want to return a random number between 1 and 100 to match Ada-rando. This is trivial with the built-in predicate random_between/3. We write the result and are done.

If an argument is passed, this predicate will not resolve, and Prolog will proceed to the next main/1 clause in the procedure for main/1. 
*/ 
main([]) :-
    random_between(1, 100, R),
    writeln(R).

/*
Here we have used syntax so that ArgAtom will unify with a single arg only. If you pass multiple args, this predicate will not resolve, and the Prolog engine will keep searching. When it does resolve, the predicate prints a random integer between 1 and the number in the arg. For example, to get a random number between 1 and 1000, use:

./rando 1000
*/
main([ArgAtom]) :-
    atom_number(ArgAtom, ArgNum),
    random_between(1, ArgNum, R),
    writeln(R).

/* 
The third and final iteration is two numbers passed: lower bound and upper bound. This is handled similarly to the single argument above. This version expects a list of precisely two items which are represented by the variables ArgAtom1 and ArgAtom2. After some nifty conversion to ArgNum1 and ArgNum2, we pick a random integer between the two limits and print it. 
*/
main([ArgAtom1, ArgAtom2]) :-
    atom_number(ArgAtom1, ArgNum1),
    atom_number(ArgAtom2, ArgNum2),
    random_between(ArgNum1, ArgNum2, R),
    writeln(R).

/*
And here is the Ada code. It is daunting. 

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;

procedure Rando is
   Low : Integer := 1;
   High : Integer := 100;
begin
   if Ada.Command_Line.Argument_Count = 1 then
      High := Integer'Value(Ada.Command_Line.Argument(1));
   end if;
   Put_Line ("rando - by Joe Legner");
   declare
      subtype Rand_Range is Integer range Low..High;
      package Rand_Int is new ada.numerics.discrete_random(Rand_Range);
      use Rand_Int;
      G : Generator;
      Answer : Rand_Range;
   begin
      Reset (G);
      Answer := Random(G);
      Put_Line ("Random number from" & Low'Image & " to" & High'Image & ":");
      Put_Line (Answer'Image);
   end;
   exception
      When Constraint_Error => Put_Line ("Argument must be a positive integer.");
end Rando;
```

And here is a listing of the project file `rando.gpr`

```ada
project Rando is
    for Source_Dirs use ("src");
    for Object_Dir use "obj";
    for Exec_Dir use "bin";
    for Main use ("rando.adb");
end Rando;
```

Usage:

```zsh
$ rando     # default 1-100
rando - by Joe Legner
Random number from 1 to 100:
 90
```

You can use an integer command line argument $X$ to generate a random number between 1 and $X$.

```zsh
$ rando 12
rando - by Joe Legner
Random number from 1 to 12:
 1
```
*/