% forall.pl
% Let's look at forall/2.
%
% Here is the SWI-Prolog documentation on forall/2:
%
% forall(Cond, Action)
%
% "For all alternative bindings of Cond, Action can be proven. 
% The example verifies that all arithmetic statements in the
% given list are correct. It does not say which is wrong if one
% proves wrong."
%
% Notes:
% 1. forall/2 does not create variable bindings. If that's what 
%    you want, look instead at maplist/2, findall/3, or foreach/2.

% IMPORTANT NOTE: USE forall/n INSTEAD OF FAILURE DRIVEN LOOPS.
% Reason given here:
% https://logtalk.org/2019/08/02/failure-driven-loops-when-and-how.html
%
% Consider these pets
pet(maggie).
pet(rocky).
pet(josey).

title_case(Name, TitleCase) :-
    atom_codes(Name, [LowerCase| Codes]),
    0'a =< LowerCase, LowerCase =< 0'z,
    UpperCase is 0'A + LowerCase - 0'a,
    atom_codes(TitleCase, [UpperCase| Codes]).

write_pet_names :-
    forall(
        pet(Name),
        (title_case(Name, TitleCase), write(TitleCase), nl)
    ).

% Example usage:
% 
% ?- write_pet_names.
% Maggie
% Rocky
% Josey
% true.