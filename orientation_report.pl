% orientation_report.pl

:- dynamic cell/3.

canvas(rows, 48).
canvas(columns, 104).

rows(R) :- 
    canvas(rows, R).

columns(C) :- 
    canvas(columns, C).

% Clear canvas memory before writing
clear_canvas :-
    retractall(cell(_, _, _)).

% Place text at a specific Row and Starting Column
place_text(Row, Col, Text) :-
    atom_chars(Text, Chars),
    place_chars(Row, Col, Chars).

place_chars(_, _, []) :- !.
place_chars(Row, Col, [Char|Rest]) :-
    assertz(cell(Row, Col, Char)),
    Col1 is Col + 1,
    place_chars(Row, Col1, Rest).

% Center a string horizontally at a given row
center_text(Row, Text) :-
    columns(C),
    atom_length(Text, L),
    ColStart is ((C - L) // 2) + 1,
    place_text(Row, ColStart, Text).

% Build the page content before writing
build_page :-
    clear_canvas,
    center_text(1, 'That Piano Entertainment').

% Write the canvas to a file
write_canvas(File) :-
    build_page,
    rows(R),
    columns(C),
    open(File, write, Stream),
    write_canvas_rows(1, R, C, Stream),
    close(Stream).

write_canvas_rows(Row, MaxRow, _, _) :-
    Row > MaxRow, !.
write_canvas_rows(Row, MaxRow, MaxCol, Stream) :-
    write_canvas_row(Row, 1, MaxCol, Stream),
    nl(Stream),
    Row1 is Row + 1,
    write_canvas_rows(Row1, MaxRow, MaxCol, Stream).

write_canvas_row(_, Col, MaxCol, _) :-
    Col > MaxCol, !.
write_canvas_row(Row, Col, MaxCol, Stream) :-
    ( cell(Row, Col, Char) -> true ; Char = '.' ),
    put_char(Stream, Char),
    Col1 is Col + 1,
    write_canvas_row(Row, Col1, MaxCol, Stream).

write_canvas :- 
    write_canvas("orientation_report.txt").