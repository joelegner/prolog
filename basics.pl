% basics.pl
% This file provides basics for Prolog. It is a good starting
% point if you are just getting started with Prolog. It is
% also a good "beginner's mind" file to return to when you are
% getting plateaued.

% This is a fact. It is also a clause. A binary relation is
% written like this:
capital(illinois, springfield).

% They are also called compound terms. A compound term is formed
% by an atom followed by arguments enclosed in parentheses. 
loves(joe, julie).
loves(julie, joe).

% You can write whatever you want. It could be this way too:
has_for(joe, love, julie).
has_for(julie, love, joe).

% Read the first one as "Joe has love for Julie." The point is you
% can use Prolog however it suits you. 

% You can put the operands in whatever order you like. But
% traditionally the first argument is the source and the
% second and subsequent arguments are targets of the relation.
connects(cable1, keyboard2, mixer1).

% Each fact is secretly a rule. It has an empty body. You can
% write any rule with an explicit body like this:
likes(joe, applesauce) :- true.

% A rule has a head and a body like this:
% head :- body.

% The head of a rule is a compound term. 
