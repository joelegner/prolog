% floorplan.pl
% Floor plan drawing for Twitch setup.

:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(settings)).

% Declare setting for border line width
:- setting(border_line_width, number, 2, 'Width of border lines in points').
:- setting(title, text, '35x25 Twitch Floor Plan', 'Title of document').

% Main predicate to run everything and write to file
run :-
    phrase(page, Lines),
    open('floorplan.ps', write, Stream),
    maplist(format(Stream, '~w~n'), Lines),
    close(Stream).

page -->
    start_page,
    rotate_coords,
    set_scale,
    draw_content,
    ['showpage'].

start_page --> 
    ['%!PS-Adobe-3.0'],
    ['%%BoundingBox: 0 0 792 612'],
    { setting(title, Title),
      atomic_list_concat(['%%Title: ', Title], Line)
    },
    [Line],
    ['%%Pages: 1'],
    ['%%EndComments'].

rotate_coords --> 
    ['% Rotate coordinate system to landscape'],
    ['90 rotate'],
    ['0 -612 translate'].

set_scale -->
    { join_line(['% ', 1, ' foot = ', 18, ' points = ', 1, '/', 4, ' inch'], Result) },
    Result,
    ['/ft 18 def'].

draw_content -->
    draw_border.

draw_border -->
    ['% Box size in feet'],
    ['/width_ft 35 def'],
    ['/height_ft 25 def'],
    ['% Convert to points'],
    ['/w width_ft ft mul def    % 630 pt'],
    ['/h height_ft ft mul def   % 450 pt'],
    ['% Compute origin (lower-left corner) to center on 792x612 paper'],
    ['/x0 792 w sub 2 div def'],
    ['/y0 612 h sub 2 div def'],
    ['% Set line width'],
    { setting(border_line_width, W),
      number_string(W, WStr),
      atomic_list_concat([WStr, ' setlinewidth'], Line)
    },
    [Line],
    ['% Draw rectangle'],
    ['newpath'],
    ['x0 y0 moveto'],
    ['w 0 rlineto'],
    ['0 h rlineto'],
    ['w neg 0 rlineto'],
    ['closepath'],
    ['stroke'].

% Convert a mixed list of atoms/numbers/strings into a single string
join_line(Mixed, [Joined]) :-
    maplist(to_atom, Mixed, Parts),
    atomic_list_concat(Parts, '', Joined).

to_atom(X, Atom) :-
    (   string(X) -> Atom = X
    ;   number(X) -> number_string(X, Atom)
    ;   atom(X)   -> Atom = X
    ).
