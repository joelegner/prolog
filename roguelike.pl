:- dynamic player_position/2.
:- dynamic game_map/1.
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
    retractall(player_position(_, _)),
    retractall(game_map(_)),
    asserta(player_position(RandX, RandY)),
    generate_map(W, H, RandX, RandY, Map),
    asserta(game_map(Map)),
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

move(dx(DX), dy(DY)) :-
    player_position(X, Y),
    NX is X + DX,
    NY is Y + DY,
    map_width(W),
    map_height(H),
    NX >= 0, NX < W,
    NY >= 0, NY < H,
    retract(player_position(X, Y)),
    asserta(player_position(NX, NY)),
    generate_map(W, H, NX, NY, NewMap),
    retractall(game_map(_)),
    asserta(game_map(NewMap)),
    print_bordered_map(NewMap, NX, NY).

n   :- move(dx( 0), dy(-1)).
s   :- move(dx( 0), dy( 1)).
e   :- move(dx( 1), dy( 0)).
w   :- move(dx(-1), dy( 0)).
ne  :- move(dx( 1), dy(-1)).
nw  :- move(dx(-1), dy(-1)).
se  :- move(dx( 1), dy( 1)).
sw  :- move(dx(-1), dy( 1)).
map :- move(dx(0), dy(0)).
