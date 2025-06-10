% orientation_report.pl
:- use_module(library(date)).

:- dynamic cell/3.


% Build the page content before writing
build_page :-
    clear_canvas,
    center_text(1, 'That Piano Entertainment, LLC'),
    left_text(1, 'Weekly Orientation Report'),
    right_text(1, 'June 8 - June 11, 2025'),
    horizontal_line(2),
    market_conditions(3, 1),
    opportunities_and_threats(13, 1),
    assumptions_and_beliefs(23, 1),
    recent_actions_outcomes(33, 1),
    tactical_priorities(43, 1),
    unknowns(53),
    bottom_row(BRow),
    right_text(BRow, 'https://github.com/joelegner/prolog/blob/main/orientation_report.pl'),
    timestamp(Stamp), 
    left_text(BRow, Stamp).


canvas(rows, 48).
canvas(columns, 104).

rows(R) :- 
    canvas(rows, R).

columns(C) :- 
    canvas(columns, C).

bottom_row(R) :- 
    canvas(rows, R).

% Get current timestamp as an atom, formatted to the second
timestamp(Text) :-
    get_time(Time),
    format_time(atom(Text), '%Y-%m-%d %H:%M:%S', Time).

% Clear canvas memory before writing
clear_canvas :-
    retractall(cell(_, _, _)).

% Draw a horizontal line using box-drawing characters
horizontal_line(Row) :-
    columns(C),
    draw_horizontal_line(Row, 1, C).

draw_horizontal_line(_, Col, MaxCol) :-
    Col > MaxCol, !.
draw_horizontal_line(Row, Col, MaxCol) :-
    assertz(cell(Row, Col, '─')),
    Col1 is Col + 1,
    draw_horizontal_line(Row, Col1, MaxCol).

% Draw a box with top-left at (Row1, Col1) and bottom-right at (Row2, Col2)
% Draw a box given top-left (Row, Col), and dimensions (Width, Height)
% Example of usage:
% draw_box(13, 31, 20, 8).
draw_box(TopRow, LeftCol, Width, Height) :-
    BottomRow is TopRow + Height - 1,
    RightCol  is LeftCol + Width - 1,
    draw_box_edges(TopRow, LeftCol, BottomRow, RightCol).

% Helper to draw edges using calculated bottom-right corner
draw_box_edges(Row1, Col1, Row2, Col2) :-
    draw_horizontal_edge(Row1, Col1, Col2),         % Top edge
    draw_horizontal_edge(Row2, Col1, Col2),         % Bottom edge
    draw_vertical_edge(Col1, Row1, Row2),           % Left edge
    draw_vertical_edge(Col2, Row1, Row2),           % Right edge
    assertz(cell(Row1, Col1, '┌')),                 % Top-left corner
    assertz(cell(Row1, Col2, '┐')),                 % Top-right corner
    assertz(cell(Row2, Col1, '└')),                 % Bottom-left corner
    assertz(cell(Row2, Col2, '┘')).                 % Bottom-right corner

draw_horizontal_edge(Row, Col1, Col2) :-
    Col1 < Col2,
    ColMidStart is Col1 + 1,
    ColMidEnd is Col2 - 1,
    draw_horizontal(Row, ColMidStart, ColMidEnd).

draw_horizontal(_, C1, C2) :-
    C1 > C2, !.
draw_horizontal(Row, Col, ColEnd) :-
    assertz(cell(Row, Col, '─')),
    Col1 is Col + 1,
    draw_horizontal(Row, Col1, ColEnd).

draw_vertical_edge(Col, Row1, Row2) :-
    Row1 < Row2,
    RowMidStart is Row1 + 1,
    RowMidEnd is Row2 - 1,
    draw_vertical(Col, RowMidStart, RowMidEnd).

draw_vertical(_, R1, R2) :-
    R1 > R2, !.
draw_vertical(Col, Row, RowEnd) :-
    assertz(cell(Row, Col, '│')),
    Row1 is Row + 1,
    draw_vertical(Col, Row1, RowEnd).

% Place text at a specific Row and Starting Column
% Default: left justification
place_text(Row, Col, Text) :-
    place_text(Row, Col, Text, left_justified).

% Justified placement
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

% Core placement logic (no justification logic here)
place_text_direct(Row, Col, Text) :-
    atom_chars(Text, Chars),
    place_chars(Row, Col, Chars).

place_chars(_, _, []) :- !.
place_chars(Row, Col, [Char|Rest]) :-
    assertz(cell(Row, Col, Char)),
    Col1 is Col + 1,
    place_chars(Row, Col1, Rest).

% Place on page predicates
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

% Shortcuts for placing on a line
center_text(Row, Text) :-
    place_on_page(Row, Text, center_justified).

left_text(Row, Text) :-
    place_on_page(Row, Text, left_justified).

right_text(Row, Text) :-
    place_on_page(Row, Text, right_justified).

% Write a list of lines starting at Row, Col, using specified justification
write_lines(_, _, [], _) :- !.
write_lines(Row, Col, [Line|Rest], Justify) :-
    place_text(Row, Col, Line, Justify),
    NextRow is Row + 1,
    write_lines(NextRow, Col, Rest, Justify).

/*
Current Market Conditions: brief notes on demand trends, competitor moves, and customer feedback relevant this week
*/
market_conditions(Row, Col) :-
    Conditions = [
        'CURRENT MARKET CONDITIONS',
        '- Windish is advertising for solo piano, crew.'
    ],
    write_lines(Row, Col, Conditions, left_justified).

/*
Key Opportunities and Threats: what new prospects or risks have emerged or shifted recently
*/
opportunities_and_threats(Row, Col) :-
    Items = [
        'KEY OPPORTUNITIES AND THREATS',
        '- One-off wedding gig possible for August.',
        '- Competing agency lost a performer recently.'
    ],
    write_lines(Row, Col, Items, left_justified).

/*
Assumptions and Beliefs: any assumptions about customer preferences, seasonality, or partner reliability you’re testing or updating
*/
assumptions_and_beliefs(Row, Col) :-
    Beliefs = [
        'ASSUMPTIONS AND BELIEFS',
        '- Summer weekday gigs are harder to fill.',
        '- Weekend demand will stay strong through July.'
    ],
    write_lines(Row, Col, Beliefs, left_justified).

/*
Recent Actions and Outcomes: quick summary of last week’s initiatives and what was learned from results
*/
recent_actions_outcomes(Row, Col) :-
    Results = [
        'RECENT ACTIONS AND OUTCOMES',
        '- Tested Facebook ad — no significant response.',
        '- Sent availability survey to three musicians.'
    ],
    write_lines(Row, Col, Results, left_justified).

/*
Tactical Priorities: top 3 to 5 focus areas or decisions for the coming week based on updated understanding
*/
tactical_priorities(Row, Col) :-
    Priorities = [
        'TACTICAL PRIORITIES',
        '- Follow up with wedding inquiry.',
        '- Draft social media post for July events.',
        '- Review musician backup list.'
    ],
    write_lines(Row, Col, Priorities, left_justified).

/*
Critical Unknowns: key questions or uncertainties that could impact decisions and require observation or data gathering
*/
unknowns(Row) :-
    Lines = [
        'CRITICAL UNKNOWNS',
        '- Will June 22 booking confirm?',
        '- Will repeat client reach out for July 13?'
    ],
    write_lines(Row, 1, Lines, left_justified).

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
    ( cell(Row, Col, Char) -> true ; Char = ' ' ),
    % Uncomment this and comment above to draw grid.
    % ( cell(Row, Col, Char) -> true ; Char = '·' ),
    put_char(Stream, Char),
    Col1 is Col + 1,
    write_canvas_row(Row, Col1, MaxCol, Stream).

write_canvas :- 
    write_canvas("orientation_report.txt").
