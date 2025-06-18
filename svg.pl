#!/usr/bin/env swipl
% svg.pl
% Experiment to generate an SVG using DCGs.

:- use_module(library(dcg/basics)).
:- initialization(main, main).

main :-
    phrase(svg("TWITCH ROOM"), Codes),
    open('svg.svg', write, Stream, [encoding(utf8)]),
    format(Stream, '~s', [Codes]),
    close(Stream),
    shell('open svg.svg').

% Top-level SVG DCG
svg(Title) -->
    xml_header,
    svg_open,
    polygon_element,
    text_element(Title),
    svg_close.

% Header
xml_header -->
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n".

% SVG open tag
svg_open -->
    "<svg width=\"176in\" height=\"290in\" viewBox=\"0 0 176 290\"\n",
    "     version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n".

% Polygon
polygon_element -->
    "  <polygon points=\"0,0 176,0 176,200 86,290 0,290\"\n",
    "           fill=\"none\"\n",
    "           stroke=\"black\"\n",
    "           stroke-width=\"0.02in\" />\n".

% Text element
text_element(Title) -->
    "  <text x=\"88\" y=\"145\"\n",
    "        text-anchor=\"middle\"\n",
    "        dominant-baseline=\"middle\"\n",
    "        font-size=\"12\"\n",
    "        font-family=\"sans-serif\"\n",
    "        fill=\"lightgrey\"\n",
    "        opacity=\"0.6\">\n",
    "    ",
    string(Title),
    "\n",
    "  </text>\n".

% SVG close tag
svg_close -->
    "</svg>\n".
