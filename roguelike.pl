% roguelike.pl
/*
Dungeon Crawler Grid System with Pathfinding on 80x24 grid with coordinates (1,1) at lower-left, (80,24) at upper-right.
*/

% Grid dimensions
grid_width(80).
grid_height(24).

% Dungeon Layout with Rooms and Corridors
% Room 1: Starting room (bottom-left)
obstacle(X, Y) :- room1_walls(X, Y).
room1_walls(5, Y) :- Y >= 1, Y =< 8.     % Right wall
room1_walls(X, 8) :- X >= 2, X =< 6.     % Top wall
room1_walls(X, 1) :- X >= 6, X =< 15.    % Bottom extension

% Corridor 1: Horizontal passage
obstacle(X, Y) :- corridor1_walls(X, Y).
corridor1_walls(X, 4) :- X >= 6, X =< 20. % Top wall of corridor
corridor1_walls(X, 2) :- X >= 6, X =< 20. % Bottom wall of corridor

% Room 2: Middle-left room
obstacle(X, Y) :- room2_walls(X, Y).
room2_walls(15, Y) :- Y >= 5, Y =< 12.    % Left wall
room2_walls(25, Y) :- Y >= 5, Y =< 12.    % Right wall  
room2_walls(X, 12) :- X >= 15, X =< 25.   % Top wall
room2_walls(X, 5) :- X >= 15, X =< 18.    % Bottom wall (with gap)
room2_walls(X, 5) :- X >= 22, X =< 25.    % Bottom wall (with gap)

% Corridor 2: Vertical passage up
% obstacle(X, Y) :- corridor2_walls(X, Y).
corridor2_walls(19, Y) :- Y >= 13, Y =< 18. % Left wall
corridor2_walls(21, Y) :- Y >= 13, Y =< 18. % Right wall

% Room 3: Upper-middle room
obstacle(X, Y) :- room3_walls(X, Y).
room3_walls(15, Y) :- Y >= 18, Y =< 24.   % Left wall
room3_walls(35, Y) :- Y >= 18, Y =< 24.   % Right wall
room3_walls(X, 24) :- X >= 15, X =< 35.   % Top wall
room3_walls(X, 18) :- X >= 15, X =< 18.   % Bottom wall (with gap)
room3_walls(X, 18) :- X >= 22, X =< 32.   % Bottom wall (with gap)

% Corridor 3: Horizontal passage right
obstacle(X, Y) :- corridor3_walls(X, Y).
corridor3_walls(X, 22) :- X >= 36, X =< 50. % Top wall
corridor3_walls(X, 20) :- X >= 36, X =< 50. % Bottom wall

% Room 4: Right-side room
obstacle(X, Y) :- room4_walls(X, Y).
room4_walls(45, Y) :- Y >= 15, Y =< 19.   % Left wall
room4_walls(60, Y) :- Y >= 15, Y =< 19.   % Right wall
room4_walls(X, 19) :- X >= 45, X =< 60.   % Top wall
room4_walls(X, 15) :- X >= 45, X =< 48.   % Bottom wall (with gap)
room4_walls(X, 15) :- X >= 52, X =< 60.   % Bottom wall (with gap)

% Corridor 4: Down and right to final room
obstacle(X, Y) :- corridor4_walls(X, Y).
corridor4_walls(55, Y) :- Y >= 10, Y =< 14. % Vertical part
corridor4_walls(57, Y) :- Y >= 10, Y =< 14. % Vertical part
corridor4_walls(X, 10) :- X >= 58, X =< 75. % Horizontal part top
corridor4_walls(X, 8) :- X >= 58, X =< 75.  % Horizontal part bottom

% Final Room: Goal area (upper-right)
obstacle(X, Y) :- final_room_walls(X, Y).
final_room_walls(70, Y) :- Y >= 15, Y =< 22. % Left wall
final_room_walls(80, Y) :- Y >= 15, Y =< 23. % Right wall (gap at top)
final_room_walls(X, 15) :- X >= 70, X =< 80. % Bottom wall
final_room_walls(X, 24) :- X >= 70, X =< 79. % Top wall (gap at corner)

% Some interior obstacles for interest
obstacle(8, 6).   % Room 1 obstacle
obstacle(9, 6).
obstacle(22, 9).  % Room 2 obstacle
obstacle(23, 9).
obstacle(28, 21). % Room 3 obstacle  
obstacle(29, 21).
obstacle(52, 17). % Room 4 obstacle
obstacle(53, 17).
obstacle(75, 18). % Final room obstacle
obstacle(76, 18).

% Valid position check (must not be obstacle)
valid_pos(X, Y) :-
    grid_width(W),
    grid_height(H),
    X >= 1, X =< W,
    Y >= 1, Y =< H,
    \+ obstacle(X, Y).

% Movement directions (8-directional including diagonals)
move(up, 0, 1).
move(down, 0, -1).
move(left, -1, 0).
move(right, 1, 0).
move(up_left, -1, 1).
move(up_right, 1, 1).
move(down_left, -1, -1).
move(down_right, 1, -1).

% Calculate next position
next_pos(X, Y, Direction, NextX, NextY) :-
    move(Direction, DX, DY),
    NextX is X + DX,
    NextY is Y + DY,
    valid_pos(NextX, NextY).

% Manhattan distance calculation (kept for reference)
manhattan_distance(X, Y, GoalX, GoalY, H) :-
    H is abs(X - GoalX) + abs(Y - GoalY).

% Optimal path with diagonal movement and obstacle avoidance
% Now we need A* since there are obstacles
find_path(StartX, StartY, GoalX, GoalY, Path) :-
    astar_search([[0, 0, StartX, StartY, []]], GoalX, GoalY, [], Path).

% A* search with open and closed lists
astar_search([[_, G, X, Y, PathSoFar]|_], GoalX, GoalY, _, FinalPath) :-
    X == GoalX, Y == GoalY, !,
    reverse([(X,Y)|PathSoFar], FinalPath).

astar_search([[F, G, X, Y, PathSoFar]|OpenRest], GoalX, GoalY, Closed, FinalPath) :-
    \+ member((X,Y), Closed),
    findall([NewF, NewG, NextX, NextY, [(X,Y)|PathSoFar]],
            (move(_, DX, DY),
             NextX is X + DX,
             NextY is Y + DY,
             valid_pos(NextX, NextY),
             \+ member((NextX, NextY), [(X,Y)|PathSoFar]),
             \+ member((NextX, NextY), Closed),
             NewG is G + 1,
             manhattan_distance(NextX, NextY, GoalX, GoalY, H),
             NewF is NewG + H),
            Successors),
    append(OpenRest, Successors, NewOpen),
    sort(NewOpen, SortedOpen),
    astar_search(SortedOpen, GoalX, GoalY, [(X,Y)|Closed], FinalPath).

astar_search([_|OpenRest], GoalX, GoalY, Closed, FinalPath) :-
    astar_search(OpenRest, GoalX, GoalY, Closed, FinalPath).

% Display the grid with player at specific position and breadcrumbs for visited path
display_grid_with_breadcrumbs(PlayerX, PlayerY, VisitedPositions) :-
    grid_height(H),
    display_rows_with_breadcrumbs(H, PlayerX, PlayerY, VisitedPositions).

display_rows_with_breadcrumbs(0, _, _, _) :- !.
display_rows_with_breadcrumbs(Row, PlayerX, PlayerY, VisitedPositions) :-
    Row > 0,
    display_row_with_breadcrumbs(1, Row, PlayerX, PlayerY, VisitedPositions),
    nl,
    NextRow is Row - 1,
    display_rows_with_breadcrumbs(NextRow, PlayerX, PlayerY, VisitedPositions).

display_row_with_breadcrumbs(Col, Row, PlayerX, PlayerY, VisitedPositions) :-
    grid_width(W),
    Col > W, !.
display_row_with_breadcrumbs(Col, Row, PlayerX, PlayerY, VisitedPositions) :-
    Col =< 80,
    (   (Col == PlayerX, Row == PlayerY) ->
        write('\033[1;37m@\033[0m')          % Player: Bold White
    ;   obstacle(Col, Row) ->
        write('\033[0;37m#\033[0m')          % Obstacles: Light Gray
    ;   member((Col, Row), VisitedPositions) ->
        write('\033[0;34m,\033[0m')          % Breadcrumbs: Dark Blue
    ;   write('\033[1;30m.\033[0m')          % Empty space: Dark Gray
    ),
    NextCol is Col + 1,
    display_row_with_breadcrumbs(NextCol, Row, PlayerX, PlayerY, VisitedPositions).

% Get visited positions up to a certain fraction of the path
get_visited_positions(Path, Fraction, VisitedPositions) :-
    length(Path, Len),
    Index is max(1, min(Len, round(Len * Fraction))),
    length(VisitedPrefix, Index),
    append(VisitedPrefix, _, Path),
    VisitedPrefix = VisitedPositions.

% Get position at fraction of path
get_position_at_fraction(Path, Fraction, X, Y) :-
    length(Path, Len),
    Index is max(1, min(Len, round(Len * Fraction))),
    nth1(Index, Path, (X, Y)).

% Main demonstration predicate
demo_pathfinding :-
    % Start at lower-left (1,1), goal at upper-right (80,24)
    StartX = 1, StartY = 1,
    GoalX = 80, GoalY = 24,
    
    write('\033[1;36mFinding path from (1,1) to (80,24)...\033[0m'), nl,
    find_path(StartX, StartY, GoalX, GoalY, Path),
    length(Path, PathLen),
    format('\033[1;36mPath found with ~w steps\033[0m~n~n', [PathLen]),
    
    % Display at 1/4 progress
    write('\033[1;35m=== 1/4 Progress ===\033[0m'), nl,
    get_position_at_fraction(Path, 0.25, X1, Y1),
    get_visited_positions(Path, 0.25, Visited1),
    format('\033[1;35mPlayer at (~w,~w)\033[0m~n', [X1, Y1]),
    display_grid_with_breadcrumbs(X1, Y1, Visited1),
    nl,
    
    % Display at 1/2 progress
    write('\033[1;35m=== 1/2 Progress ===\033[0m'), nl,
    get_position_at_fraction(Path, 0.5, X2, Y2),
    get_visited_positions(Path, 0.5, Visited2),
    format('\033[1;35mPlayer at (~w,~w)\033[0m~n', [X2, Y2]),
    display_grid_with_breadcrumbs(X2, Y2, Visited2),
    nl,
    
    % Display at 3/4 progress
    write('\033[1;35m=== 3/4 Progress ===\033[0m'), nl,
    get_position_at_fraction(Path, 0.75, X3, Y3),
    get_visited_positions(Path, 0.75, Visited3),
    format('\033[1;35mPlayer at (~w,~w)\033[0m~n', [X3, Y3]),
    display_grid_with_breadcrumbs(X3, Y3, Visited3),
    nl,
    
    % Display at full progress (goal)
    write('\033[1;35m=== Full Progress (Goal Reached) ===\033[0m'), nl,
    format('\033[1;35mPlayer at (~w,~w)\033[0m~n', [GoalX, GoalY]),
    display_grid_with_breadcrumbs(GoalX, GoalY, Path),
    nl.

% Alternative: Show the complete path
show_complete_path :-
    find_path(1, 1, 80, 24, Path),
    write('Complete path: '), nl,
    print_path(Path).

print_path([]).
print_path([(X,Y)|Rest]) :-
    format('(~w,~w) ', [X, Y]),
    print_path(Rest).

% Show grid with obstacles (for debugging)
show_empty_grid :-
    write('\033[1;34m=== Empty Grid with Obstacles ===\033[0m'), nl,
    display_grid_with_breadcrumbs(1, 1, []),
    nl.

% Color legend display
show_color_legend :-
    write('\033[1;34m=== Color Legend ===\033[0m'), nl,
    write('\033[1;37m@\033[0m - Player (Bold White)'), nl,
    write('\033[0;37m#\033[0m - Obstacles/Walls (Light Gray)'), nl,
    write('\033[0;34m,\033[0m - Breadcrumbs/Trail (Dark Blue)'), nl,
    write('\033[1;30m.\033[0m - Empty Space (Dark Gray)'), nl,
    nl.

/* Query examples:

```prolog
?- demo_pathfinding.
?- find_path(1, 1, 80, 24, Path).
?- display_grid_with_breadcrumbs(40, 12, [(1,1), (2,2), (3,3)]).
?- show_complete_path.
?- show_empty_grid.
?- show_color_legend.
```
*/

/*
=== Full Progress (Goal Reached) ===
Player at (80,24)
..............#####################..................................##########@
..............#...................#..................................,,,,,,,,,,#
..............#...................################..................,#.........#
..............#............##.....#.................................,#.........#
..............#...................################..................,#.........#
..............#...................#.........################........,#.........#
..............####...###########..#.........#..............#........,#....##...#
............................................#......##......#........,#.........#
............................................#..............#........,#.........#
............................................####...#########........,###########
......................................................#.#............,..........
.....,,,,,,,,,,,,,,,,,,,,.............................#.#.............,.........
....,.........###########,............................#.#..............,........
...,..........#.........#.,...........................#.#...............,,,.....
..,...........#.........#..,..........................#.###################,....
.,............#......##.#...,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,.....
,#####........#.........#................................##################.....
,...#.........#.........#.......................................................
,...#..##.....#.........#.......................................................
,...#.........####...####.......................................................
,...################............................................................
,...#...........................................................................
,...################............................................................
,...###########.................................................................

*/