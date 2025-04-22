:- use_module(library(random)).

% Size of the map (excluding border)
map_width(24).
map_height(24).

% Entry point
start_game :-
    map_width(W),
    map_height(H),
    H1 is H - 1,
    W1 is W - 1,
    random_between(0, H1, RandY),
    random_between(0, W1, RandX),
    generate_map(W, H, RandX, RandY, Map),
    print_bordered_map(Map, RandX, RandY).

% Generate the full map with a character at (CharX, CharY)
generate_map(W, H, CharX, CharY, Map) :-
    generate_rows(0, H, W, CharX, CharY, Map).

generate_rows(H, H, _, _, _, []) :- !.
generate_rows(Y, MaxY, W, CharX, CharY, [Row|Rest]) :-
    generate_row(0, W, CharX, CharY, Y, Row),
    Y1 is Y + 1,
    generate_rows(Y1, MaxY, W, CharX, CharY, Rest).

generate_row(W, W, _, _, _, []) :- !.
generate_row(X, MaxX, CharX, CharY, CurrY, [Cell|Rest]) :-
    (X =:= CharX, CurrY =:= CharY -> Cell = '@' ; Cell = '.'),
    X1 is X + 1,
    generate_row(X1, MaxX, CharX, CharY, CurrY, Rest).

% Print the map with a Code Page 437 border and player position
print_bordered_map(Map, CharX, CharY) :-
    map_width(W),
    WChars is W * 2 - 1, % Adjust for spaces between characters
    print_border_top(WChars),
    print_map_with_sides(Map),
    print_border_bottom(WChars),
    format("@(~d, ~d)~n", [CharX, CharY]).

print_border_top(Len) :-
    write('╔'),
    print_chars('═', Len),
    write('╗'), nl.

print_border_bottom(Len) :-
    write('╚'),
    print_chars('═', Len),
    write('╝'), nl.

print_map_with_sides([]).
print_map_with_sides([Row|Rest]) :-
    write('║'),
    print_row(Row),
    write('║'), nl,
    print_map_with_sides(Rest).

print_row([Cell]) :-  % Last cell, no trailing space
    write(Cell).
print_row([Cell|Rest]) :-
    write(Cell), write(' '),
    print_row(Rest).

print_chars(_, 0) :- !.
print_chars(Char, N) :-
    write(Char),
    N1 is N - 1,
    print_chars(Char, N1).

squares(N) :-
    map_width(W),
    map_height(H),
    N is W*H.
