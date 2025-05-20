% crossword.pl
% See Ivan Bratko book Prolog Programming for Artificial Intelligence,
% Section 1.8.1.

word(d,o,g). 
word(r,u,n). 
word(t,o,p).
word(f,o,u,r). 
word(l,o,s,t). 
word(m,e,s,s).
word(b,a,k,e,r). 
word(f,o,r,u,m). 
word(g,r,e,e,n).
word(p,r,o,l,o,g).  % ← FIXED TYPO
word(v,a,n,i,s,h). 
word(w,o,n,d,e,r).

solution(L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16) :-
    word(L1,L2,L3,L4,L5),          % Top horizontal word
    word(L9,L10,L11,L12,L13,L14),  % Second horizontal word
    word(L1,L6,L9,L15),            % First vertical word
    word(L3,L7,L11),               % Second vertical word
    word(L5,L8,L13,L16).           % Third vertical word

% ?- solution(L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16).
% Does not work. Disappointed.
