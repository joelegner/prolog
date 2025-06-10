% orientation_report.pl
:- use_module(library(date)).

:- dynamic cell/3.

% Canvas settings
canvas(rows, 48).
canvas(columns, 104).

rows(R) :- canvas(rows, R).
columns(C) :- canvas(columns, C).
bottom_row(R) :- canvas(rows, R).

% Timestamp formatting
timestamp(Text) :-
    get_time(Time),
    format_time(atom(Text), '%Y-%m-%d %H:%M:%S', Time).

% Clear canvas memory before writing
clear_canvas :- retractall(cell(_, _, _)).

% Horizontal line drawing
horizontal_line(Row) :-
    columns(C),
    draw_horizontal_line(Row, 1, C).

draw_horizontal_line(_, Col, MaxCol) :- Col > MaxCol, !.
draw_horizontal_line(Row, Col, MaxCol) :-
    assertz(cell(Row, Col, '─')),
    Col1 is Col + 1,
    draw_horizontal_line(Row, Col1, MaxCol).

% Box drawing (not used in this version but retained for extensibility)
draw_box(TopRow, LeftCol, Width, Height) :-
    BottomRow is TopRow + Height - 1,
    RightCol  is LeftCol + Width - 1,
    draw_box_edges(TopRow, LeftCol, BottomRow, RightCol).

draw_box_edges(Row1, Col1, Row2, Col2) :-
    draw_horizontal_edge(Row1, Col1, Col2),
    draw_horizontal_edge(Row2, Col1, Col2),
    draw_vertical_edge(Col1, Row1, Row2),
    draw_vertical_edge(Col2, Row1, Row2),
    assertz(cell(Row1, Col1, '┌')),
    assertz(cell(Row1, Col2, '┐')),
    assertz(cell(Row2, Col1, '└')),
    assertz(cell(Row2, Col2, '┘')).

draw_horizontal_edge(Row, Col1, Col2) :-
    Col1 < Col2,
    ColMidStart is Col1 + 1,
    ColMidEnd is Col2 - 1,
    draw_horizontal(Row, ColMidStart, ColMidEnd).

draw_horizontal(_, C1, C2) :- C1 > C2, !.
draw_horizontal(Row, Col, ColEnd) :-
    assertz(cell(Row, Col, '─')),
    Col1 is Col + 1,
    draw_horizontal(Row, Col1, ColEnd).

draw_vertical_edge(Col, Row1, Row2) :-
    Row1 < Row2,
    RowMidStart is Row1 + 1,
    RowMidEnd is Row2 - 1,
    draw_vertical(Col, RowMidStart, RowMidEnd).

draw_vertical(_, R1, R2) :- R1 > R2, !.
draw_vertical(Col, Row, RowEnd) :-
    assertz(cell(Row, Col, '│')),
    Row1 is Row + 1,
    draw_vertical(Col, Row1, RowEnd).

% Text placement
place_text(Row, Col, Text) :- place_text(Row, Col, Text, left_justified).

place_text(Row, Col, Text, left_justified) :-
    place_text_direct(Row, Col, Text).
place_text(Row, Col, Text, right_justified) :-
    atom_length(Text, Len),
    StartCol is Col - Len + 1,
    place_text_direct(Row, StartCol, Text).
place_text(Row, Col, Text, center_justified) :-
    atom_length(Text, Len),
    HalfLen is Len // 2,
    StartCol is Col - HalfLen,
    place_text_direct(Row, StartCol, Text).

place_text_direct(Row, Col, Text) :-
    atom_chars(Text, Chars),
    place_chars(Row, Col, Chars).

place_chars(_, _, []) :- !.
place_chars(Row, Col, [Char|Rest]) :-
    assertz(cell(Row, Col, Char)),
    Col1 is Col + 1,
    place_chars(Row, Col1, Rest).

place_on_page(Row, Text, left_justified) :-
    place_text(Row, 1, Text, left_justified).
place_on_page(Row, Text, right_justified) :-
    columns(C),
    atom_length(Text, Len),
    StartCol is C - Len + 1,
    place_text(Row, StartCol, Text, left_justified).
place_on_page(Row, Text, center_justified) :-
    columns(C),
    atom_length(Text, Len),
    StartCol is ((C - Len) // 2) + 1,
    place_text(Row, StartCol, Text, left_justified).

center_text(Row, Text) :- place_on_page(Row, Text, center_justified).
left_text(Row, Text) :- place_on_page(Row, Text, left_justified).
right_text(Row, Text) :- place_on_page(Row, Text, right_justified).

% Write exactly 5 lines with justification, padding with blanks if needed
write_section(RowStart, Col, Lines, RowEnd) :-
    pad_to_five(Lines, Padded),
    write_lines(RowStart, Col, Padded, left_justified),
    RowEnd is RowStart + 5.

pad_to_five(Lines, Padded) :-
    length(Lines, L),
    Extra is max(0, 5 - L),
    blank_lines(Extra, Blanks),
    append(Lines, Blanks, Padded).

blank_lines(0, []) :- !.
blank_lines(N, [''|Rest]) :-
    N1 is N - 1,
    blank_lines(N1, Rest).

write_lines(_, _, [], _) :- !.
write_lines(Row, Col, [Line|Rest], Justify) :-
    place_text(Row, Col, Line, Justify),
    NextRow is Row + 1,
    write_lines(NextRow, Col, Rest, Justify).

% Section definitions
market_conditions(RowIn, Col, RowOut) :-
    Lines = [
        'CURRENT MARKET CONDITIONS',
        '- Windish is advertising for solo piano, crew.'
    ],
    write_section(RowIn, Col, Lines, RowOut).

opportunities_and_threats(RowIn, Col, RowOut) :-
    Lines = [
        'KEY OPPORTUNITIES AND THREATS',
        '- One-off wedding gig possible for August.',
        '- Competing agency lost a performer recently.'
    ],
    write_section(RowIn, Col, Lines, RowOut).

assumptions_and_beliefs(RowIn, Col, RowOut) :-
    Lines = [
        'ASSUMPTIONS AND BELIEFS',
        '- Summer weekday gigs are harder to fill.',
        '- Weekend demand will stay strong through July.'
    ],
    write_section(RowIn, Col, Lines, RowOut).

recent_actions_outcomes(RowIn, Col, RowOut) :-
    Lines = [
        'RECENT ACTIONS AND OUTCOMES',
        '- Tested Facebook ad — no significant response.',
        '- Sent availability survey to three musicians.'
    ],
    write_section(RowIn, Col, Lines, RowOut).

tactical_priorities(RowIn, Col, RowOut) :-
    Lines = [
        'TACTICAL PRIORITIES',
        '- Follow up with wedding inquiry.',
        '- Draft social media post for July events.',
        '- Review musician backup list.'
    ],
    write_section(RowIn, Col, Lines, RowOut).

unknowns(RowIn, Col, RowOut) :-
    Lines = [
        'CRITICAL UNKNOWNS',
        '- Will June 22 booking confirm?',
        '- Will repeat client reach out for July 13?'
    ],
    write_section(RowIn, Col, Lines, RowOut).

% The stack of sections, each 5 lines tall with 1-line spacing
the_stack(RowIn, Col) :-
    market_conditions(RowIn, Col, R1),
    R2 is R1 + 1,
    opportunities_and_threats(R2, Col, R3),
    R4 is R3 + 1,
    assumptions_and_beliefs(R4, Col, R5),
    R6 is R5 + 1,
    recent_actions_outcomes(R6, Col, R7),
    R8 is R7 + 1,
    tactical_priorities(R8, Col, R9),
    R10 is R9 + 1,
    unknowns(R10, Col, _).

% Build the page before writing
build_page :-
    clear_canvas,
    center_text(1, 'That Piano Entertainment, LLC'),
    left_text(1, 'Weekly Orientation Report'),
    right_text(1, 'June 8 - June 11, 2025'),
    horizontal_line(2),
    the_stack(3, 1),
    bottom_row(BRow),
    right_text(BRow, 'https://github.com/joelegner/prolog/blob/main/orientation_report.pl'),
    timestamp(Stamp), 
    left_text(BRow, Stamp).

% Write canvas to file
write_canvas(File) :-
    build_page,
    rows(R),
    columns(C),
    open(File, write, Stream),
    write_canvas_rows(1, R, C, Stream),
    close(Stream).

write_canvas :-
    write_canvas("orientation_report.txt").

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
    ( cell(Row, Col, Char) -> true ; Char = ' ' ),
    put_char(Stream, Char),
    Col1 is Col + 1,
    write_canvas_row(Row, Col1, MaxCol, Stream).
