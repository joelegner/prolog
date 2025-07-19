%% roguelike.pl

% This was written by Claude AI free on July 18, 2025.

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
obstacle(25, 24).
obstacle(25, 23).
obstacle(25, 22).
obstacle(25, 21).
obstacle(25, 20).

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
obstacle(70, 20).
obstacle(71, 20).
obstacle(72, 20).
obstacle(73, 20).
obstacle(74, 20).

% Wall pattern 6: Near goal area
obstacle(75, 24).
obstacle(75, 23).
obstacle(75, 22).
obstacle(76, 22).
obstacle(77, 22).
obstacle(78, 22).
obstacle(79, 22).

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
        write('@')                           % Current player position
    ;   obstacle(Col, Row) ->
        write('#')                           % Obstacle/wall
    ;   member((Col, Row), VisitedPositions) ->
        write('*')                           % Breadcrumb for visited position
    ;   write('.')                           % Empty space
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
    
    write('Finding path from (1,1) to (80,24)...'), nl,
    find_path(StartX, StartY, GoalX, GoalY, Path),
    length(Path, PathLen),
    format('Path found with ~w steps~n~n', [PathLen]),
    
    % Display at 1/4 progress
    write('=== 1/4 Progress ==='), nl,
    get_position_at_fraction(Path, 0.25, X1, Y1),
    get_visited_positions(Path, 0.25, Visited1),
    format('Player at (~w,~w)~n', [X1, Y1]),
    display_grid_with_breadcrumbs(X1, Y1, Visited1),
    nl,
    
    % Display at 1/2 progress
    write('=== 1/2 Progress ==='), nl,
    get_position_at_fraction(Path, 0.5, X2, Y2),
    get_visited_positions(Path, 0.5, Visited2),
    format('Player at (~w,~w)~n', [X2, Y2]),
    display_grid_with_breadcrumbs(X2, Y2, Visited2),
    nl,
    
    % Display at 3/4 progress
    write('=== 3/4 Progress ==='), nl,
    get_position_at_fraction(Path, 0.75, X3, Y3),
    get_visited_positions(Path, 0.75, Visited3),
    format('Player at (~w,~w)~n', [X3, Y3]),
    display_grid_with_breadcrumbs(X3, Y3, Visited3),
    nl,
    
    % Display at full progress (goal)
    write('=== Full Progress (Goal Reached) ==='), nl,
    format('Player at (~w,~w)~n', [GoalX, GoalY]),
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
    write('=== Empty Grid with Obstacles ==='), nl,
    display_grid_with_breadcrumbs(1, 1, []),
    nl.

% Query examples:
% ?- demo_pathfinding.
% ?- find_path(1, 1, 80, 24, Path).
% ?- display_grid_with_breadcrumbs(40, 12, [(1,1), (2,2), (3,3)]).
% ?- show_complete_path.
% ?- show_empty_grid.