% Dungeon Crawler Grid System with Pathfinding
% 80x24 grid with coordinates (1,1) at lower-left, (80,24) at upper-right

% Grid dimensions
grid_width(80).
grid_height(24).

% Define obstacles and walls using # symbol
% Wall pattern 1: Horizontal wall near top
obstacle(10, 20).
obstacle(11, 20).
obstacle(12, 20).
obstacle(13, 20).

% Wall pattern 2: Vertical wall with gap
obstacle(25, 15).
obstacle(25, 14).
obstacle(25, 13).
obstacle(25, 12).
obstacle(25, 11).

% Wall pattern 3: L-shaped obstacle
obstacle(40, 10).
obstacle(41, 10).
obstacle(42, 10).
obstacle(42, 11).
obstacle(42, 12).

% Wall pattern 4: Scattered blocks
obstacle(15, 8).
obstacle(16, 8).
obstacle(18, 8).
obstacle(30, 18).
obstacle(31, 18).
obstacle(32, 18).
obstacle(50, 5).
obstacle(51, 5).
obstacle(52, 5).
obstacle(60, 15).
obstacle(61, 15).
obstacle(62, 15).
obstacle(63, 15).

% Wall pattern 5: Near goal area
obstacle(70, 24).
obstacle(71, 24).
obstacle(71, 23).
obstacle(72, 22).
obstacle(73, 21).
obstacle(74, 20).
obstacle(74, 20).
obstacle(75, 20).
obstacle(76, 20).
obstacle(77, 20).

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

% Query examples:
% ?- demo_pathfinding.
% ?- find_path(1, 1, 80, 24, Path).
% ?- display_grid_with_breadcrumbs(40, 12, [(1,1), (2,2), (3,3)]).
% ?- show_complete_path.
% ?- show_empty_grid.
% ?- show_color_legend.