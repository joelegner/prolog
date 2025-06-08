% failure_driven_loops.pl
% IMPORTANT NOTE: USE forall/n INSTEAD OF FAILURE DRIVEN LOOPS.
% Reason given here:
% https://logtalk.org/2019/08/02/failure-driven-loops-when-and-how.html
%
% Consider these pets
pet(maggie).
pet(rocky).
pet(josey).
%
% We want to write all their names at once without
% typing the ';' character each time at an interactive
% prompt. How this is done is something called in the 
% literature 'FAILURE DRIVEN LOOPS'. The reason it is 
% called that is the presence of the 'fail'
write_pet_names :-
    pet(Name),
    write(Name), nl,
    fail. % This is the key!

% Suppose we query our database like this:
% ?- write_pet_names.
%
% How does the Prolog engine proceed?
% 
% 1. It tries to first unify pet(Name). Lookin in the
%    facts and rules it has available, it finds the first
%    value of Name that will satisfy the predicate. This 
%    turns out to be maggie, because maggie is first at the
%    top of the file. 
%
% 2. Now that the predicate (with pet name substituted) holds,
%    the engine calls the second predicate to print the name and
%    a newline.
%
% 3. The engine comes to the fail/0 atom. Docs: 'Always fail. The 
%    predicate fail/0 is translated into a single virtual machine
%    instruction.' When it encounters this, it is led to believe 
%    that the predicate write_pet_names/0 has failed. This triggers
%    back-tracking.
%
% 4. The engine starts backtracking. It moves back up to pet/1
%    and moves down the list to rocky/0. It substitutes rocky/0 for
%    the variable Name. Now go back to 2 above. This all stops when 
%    the engine runs out of pet names and stops with false.
%
% Here is what it looks like when run:
%
% ?- write_pet_names.
% maggie
% rocky
% josey
% false.
%
% Good. It did exactly what we wanted.
%
% ***FAILURE DRIVEN LOOPS***
% Takeaway: Add fail/0 at the end of a long predicate to force the
% Prolog engine to backtrack and find more solutions. 
%
%
% More information:
% https://www.swi-prolog.org/pldoc/doc_for?object=fail/0