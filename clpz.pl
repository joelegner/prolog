% clpz.pl
% Module CLP(Z). Requires scryer-prolog.

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
*/

positive_integer(N) :- 
    N #> 0.