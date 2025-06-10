% orientation_report.pl

canvas(rows, 48).
canvas(columns, 104).

rows(R) :- 
    canvas(rows, R).

columns(C) :- 
    canvas(columns, C).

write_canvas :-
    write_canvas("orientation_report.txt").

write_canvas(File) :-
    rows(R),
    columns(C),
    open(File, write, Stream),
    write_canvas_rows(R, C, Stream),
    close(Stream).

write_canvas_rows(0, _, _) :- !.
write_canvas_rows(R, C, Stream) :-
    write_canvas_row(C, Stream),
    nl(Stream),
    R1 is R - 1,
    write_canvas_rows(R1, C, Stream).

write_canvas_row(0, _) :- !.
write_canvas_row(C, Stream) :-
    put_char(Stream, '.'),
    C1 is C - 1,
    write_canvas_row(C1, Stream).
