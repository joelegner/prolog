%% ascii_canvas.pl

/*
I have this weird dream of designing an endless text canvas. I could put text wherever I want. The main relation after the size of the canvas it the relation of row and column to character. This represents the character to display at this point. 
*/

canvas_width_height(80, 24).

row_col_char(0, 0, 'A').
row_col_char(0, 1, 'B').
row_col_char(0, 2, 'C').

row_col_char(10, 27, 'J').
row_col_char(10, 28, 'o').
row_col_char(10, 29, 'e').

visible(Row, Col) :-
    bounding_box(Points),
    Points = [row_col(Row1, Col1), row_col(Row1, Col2), row_col(Row2, Col2), row_col(Row2, Col1)],
    Row >= Row1,
    Row =< Row2,
    Col >= Col1,
    Col =< Col2.

rows(Rows) :-
    findall(Row, row_col_char(Row,_,_), Rows).

cols(Cols) :-
    findall(Col, row_col_char(_,Col,_), Cols).

min_row(MinRow) :-
    rows(Rows),
    min_list(Rows, MinRow).

max_row(MaxRow) :-
    rows(Rows),
    max_list(Rows, MaxRow).

min_col(MinCol) :-
    cols(Cols),
    min_list(Cols, MinCol).

max_col(MaxCol) :-
    cols(Cols),
    max_list(Cols, MaxCol).

bounding_box(Points) :-
    canvas_width_height(W, H),
    Points = [row_col(0, 0), row_col(0, W), row_col(H, W), row_col(H, 0)].

bounding_box(Left, Right, Top, Bottom) :-
    canvas_width_height(W, H),
    Left = line_segment([row_col(0, 0), row_col(H, 0)]),
    Right = line_segment([row_col(0, W), row_col(H, W)]),
    Top = line_segment([row_col(0, 0), row_col(0, W)]),
    Bottom = line_segment([row_col(H, 0), row_col(H, W)]).

line_segment([row_col(_, _), row_col(_, _)]).

/*
?- bounding_box(Points).
Points = [row_col(0, 0), row_col(0, 29), row_col(10, 29), row_col(10, 0)].
*/
