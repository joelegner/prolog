% clpz.pl
% Module CLP(Z). 

% Note!: Requires scryer-prolog. 
% https://www.scryer.pl/clpz
% https://www.metalevel.at/prolog/clpz

:- use_module(library(clpz)).

factorial(0, 1).
factorial(N, F) :-
    N #> 0,
    F #= F0*N,
    N1 #= N - 1,
    factorial(N1, F0).

/*
?- Y in 1..5, factorial(X, Y).
   Y = 1, X = 0
;  Y = 1, X = 1
;  Y = 2, X = 2
;  false.
*/

songcount(TotalMin, MinPerSong, N) :-
    in(N, 1..100),
    MinPerSong*N #>= TotalMin.

/*
Example usage of songcoung/3:

scryer-prolog clpz.pl
?- songcount(180, 7, N).
   clpz:(N in 26..100).
   
We get 26 songs. 26*7 = 182, and 182 > 180. Good!

It is somehow important to give the domain here. Otherwise we get 25 as a result. When the predicate did not have the domain constraint in/2, this is what happens:

songcount(TotalMin, MinPerSong, N) :-
    MinPerSong*N #>= TotalMin.

When we run this, we get 25, not 26. 

?- songcount(180, 7, N).
   clpz:(N in 25..sup), clpz:(180#=<7*N).

Since 25*7 = 175 < 180, it is not a valid solution. This is troubling. The way to make sure we get the right answer is to declare the domain. 
*/

positive_integer(N) :- 
    N #> 0.