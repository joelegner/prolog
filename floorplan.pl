% floorplan.pl
% Floor plan drawing for Twitch setup.

:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(settings)).

% Declare settings
:- setting(border_line_width, number, 2, 'Width of border lines in points').
:- setting(title, text, '35x25 Twitch Floor Plan', 'Title of document').

% Object definitions
object(couch, rectangle(10, 3.5), rotate(0)).
object(loveseat, rectangle(6, 3.5), rotate(0)).

% Object placement (feet from lower-left corner)
place(couch, 5, 0).
place(loveseat, 0, 5).

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
    draw_border,
    draw_objects.

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

draw_objects -->
    { findall(Name, object(Name, _, _), Names) },
    draw_object_list(Names).

draw_object_list([]) --> [].
draw_object_list([Name|Rest]) -->
    draw_object(Name),
    draw_object_list(Rest).

draw_object(Name) -->
    {
        object(Name, rectangle(Wft, Hft), rotate(Angle)),
        place(Name, Xft, Yft),
        Scale is 18,
        WX is Wft * Scale,
        HX is Hft * Scale,
        TX is Xft * Scale,
        TY is Yft * Scale,

        % Paper and border sizes (points)
        PaperWidth = 792,
        PaperHeight = 612,
        BorderWidth is 35 * Scale,
        BorderHeight is 25 * Scale,
        X0 is (PaperWidth - BorderWidth) / 2,
        Y0 is (PaperHeight - BorderHeight) / 2,

        % Add border origin offset to position
        XPos is TX + X0,
        YPos is TY + Y0,

        number_string(XPos, TXs),
        number_string(YPos, TYs),
        number_string(WX, WXs),
        number_string(HX, HXs),
        number_string(Angle, As),

        atomic_list_concat([TXs, ' ', TYs, ' translate'], TranslateLine),
        atomic_list_concat([As, ' rotate'], RotateLine),
        atomic_list_concat(['0 0 moveto'], MoveTo),
        atomic_list_concat([WXs, ' 0 rlineto'], RLine1),
        atomic_list_concat(['0 ', HXs, ' rlineto'], RLine2),
        atomic_list_concat(['-', WXs, ' 0 rlineto'], RLine3)
    },
    ['% Object: ~w'-[Name]],
    ['gsave'],
    [TranslateLine],
    [RotateLine],
    ['newpath'],
    [MoveTo],
    [RLine1],
    [RLine2],
    [RLine3],
    ['closepath'],
    ['stroke'],
    ['grestore'].

% Join parts into a single string
join_line(Mixed, [Joined]) :-
    maplist(to_atom, Mixed, Parts),
    atomic_list_concat(Parts, '', Joined).

to_atom(X, Atom) :-
    ( string(X) -> Atom = X
    ; number(X) -> number_string(X, Atom)
    ; atom(X)   -> Atom = X
    ).
