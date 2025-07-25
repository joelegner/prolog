% goals.pl
% Specify goal numbers and names
goal(1, 'Play the show').
goal(2, 'Turn on the keyboard.').
% goal 3 slot is open
goal(4, 'Take the keyboard out of the case.').
goal(5, 'Set the keyboard in the shell.').
goal(6, 'Plug the instrument cables into the keyboard.').
goal(7, 'Plug the sustain pedal cable into the keyboard.').
goal(8, 'Plug the power supply into the keyboard.').
goal(9, 'Plug the keyboard power supply into the power strip.').
goal(11, 'Plug the power strip into the wall.').
goal(10, 'Set the power strip in the shell with Velcro.').

% Read before(8, 2) as 'Goal 8 needs to be met before goal 2.'
before(8, 2).
before(9, 2).
before(11, 2).
before(3, 2).   
before(2, 1).
before(4, 3).
before(10, 11).
before(11, 3).
before(5, 6).
before(5, 7).
before(5, 8). 
before(5, 9).
before(9, 8).
before(10, 9).
before(2, 1).

% Make the before relation transitive
% Note we did not use the atom before this time but rather goes_before.
% This seems to be necessary to prevent infinite loops.
goes_before(X, Y) :- before(X, Y).
goes_before(X, Y) :- before(X, Z), goes_before(Z, Y).

% List all goal numbers
all_goals(Goals) :-
    findall(N, goal(N, _), Goals).

% Determine if a goal has no unmet dependencies in the list
independent(G, Goals) :-
    \+ (member(Other, Goals), goes_before(Other, G)).

% Topological sort helper
topo_sort([], []).
topo_sort(Goals, [G|Sorted]) :-
    select(G, Goals, Rest),
    independent(G, Goals),
    topo_sort(Rest, Sorted).

% Print goals in sorted order
print_sorted_goals :-
    all_goals(Goals),
    topo_sort(Goals, Sorted),
    forall(member(N, Sorted),
        (goal(N, Desc), format('~d: ~s~n', [N, Desc]))
    ).

% EXAMPLE USAGE: 
% make goals
% swipl -s goals.pl -g print_sorted_goals -t halt
% 4: Take the keyboard out of the case.
% 5: Set the keyboard in the shell.
% 6: Plug the instrument cables into the keyboard.
% 7: Plug the sustain pedal cable into the keyboard.
% 10: Set the power strip in the shell with Velcro.
% 9: Plug the keyboard power supply into the power strip.
% 8: Plug the power supply into the keyboard.
% 11: Plug the power strip into the wall.
% 2: Turn on the keyboard.
% 1: Play the show
