%% ansi_term.pl
% Demo and test file for using the SWI-Prolog ansi_term library.
% ChatGPT wrote this one.
% TODO: Explore and study this file.

:- use_module(library(ansi_term)).

% Run this file with `swipl -s ansi_term.pl -g main -t halt.`

main :-
    nl, demo_colors, nl,
    demo_backgrounds, nl,
    demo_bold_and_underline, nl,
    demo_custom_styles, nl,
    demo_format_with_ansi, nl,
    halt.

% Demonstrate some basic foreground colors
demo_colors :-
    writeln('--- Foreground Color Demo ---'),
    forall(member(Color, [red, green, yellow, blue, magenta, cyan, white]),
           ansi_format([fg(Color)], 'This is ~w text.~n', [Color])).

% Demonstrate background colors
demo_backgrounds :-
    writeln('--- Background Color Demo ---'),
    forall(member(Color, [red, green, yellow, blue, magenta, cyan, white]),
           ansi_format([bg(Color)], 'This has a ~w background.~n', [Color])).

% Demonstrate bold and underline styles
demo_bold_and_underline :-
    writeln('--- Bold and Underline Demo ---'),
    ansi_format([bold], 'This is bold text.~n', []),
    ansi_format([underline], 'This is underlined text.~n', []),
    ansi_format([bold, underline], 'This is bold and underlined.~n', []).

% Custom style tag using ansi_term:ansi_format/3
demo_custom_styles :-
    writeln('--- Custom Style Tags Demo ---'),
    Style = [fg(green), bg(black), bold],
    ansi_format(Style, 'This is custom styled: green fg, black bg, bold.~n', []).

% Demonstrate format-style output
demo_format_with_ansi :-
    writeln('--- Format Style Demo ---'),
    ansi_format([fg(blue)], 'Hello, ~w! This is ~d examples in one.~n', [user, 5]),
    ansi_format([bold, fg(yellow)], 'Emphasized: ~w.~n', ['Warning!']).

