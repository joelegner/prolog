/* 
===============================================================
These comments were copied from SWI Prolog's github repository.
===============================================================
*/

/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2024, University of Amsterdam
                              VU University Amsterdam
                              CWI Amsterdam
                              SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/** <module> Operations on atoms

This library provides operations  on  atoms   that  are  not  covered by
builtin predicates. The current implementation is   just a start, making
code developed in _xpce_ and duplicated in various projects reusable.
*/

/** <module> Print decorated text to ANSI consoles

This library allows for exploiting the color and attribute facilities of
most modern terminals using ANSI escape sequences. This library provides
the following:

  - ansi_format/3 allows writing messages to the terminal with ansi
    attributes.
  - It defines the hook prolog:message_line_element/2, which provides
    ansi attributes and hyperlinks for print_message/2.

The behavior of this library is controlled by two Prolog flags:

  - `color_term`
    When `true`, activate the color output for this library.  Otherwise
    simply call format/3.
  - `hyperlink_term`
    Emit terminal hyperlinks for url(Location) and url(URL, Label)
    elements of Prolog messages.

@see    http://en.wikipedia.org/wiki/ANSI_escape_code
*/

%!  ansi_format(+ClassOrAttributes, +Format, +Args) is det.
%
%   Format text with ANSI  attributes.   This  predicate  behaves as
%   format/2 using Format and Args, but if the =current_output= is a
%   terminal, it adds ANSI escape sequences according to Attributes.
%   For example, to print a text in bold cyan, do
%
%     ==
%     ?- ansi_format([bold,fg(cyan)], 'Hello ~w', [world]).
%     ==
%
%   Attributes is either a single attribute, a   list  thereof or a term
%   that is mapped to concrete  attributes   based  on the current theme
%   (see prolog:console_color/2). The attribute names   are derived from
%   the ANSI specification. See the source   for sgr_code/2 for details.
%   Some commonly used attributes are:
%
%     - bold
%     - underline
%     - fg(Color), bg(Color), hfg(Color), hbg(Color)
%       For fg(Color) and bg(Color), the colour name can be '#RGB' or
%       '#RRGGBB'
%     - fg8(Spec), bg8(Spec)
%       8-bit color specification.  Spec is a colour name, h(Color)
%       or an integer 0..255.
%     - fg(R,G,B), bg(R,G,B)
%       24-bit (direct color) specification.  The components are
%       integers in the range 0..255.
%
%   Defined color constants are below.  =default=   can  be  used to
%   access the default color of the terminal.
%
%     - black, red, green, yellow, blue, magenta, cyan, white
%
%   ANSI sequences are sent if and only if
%
%     - The =current_output= has the property tty(true) (see
%       stream_property/2).
%     - The Prolog flag =color_term= is =true=.

/* After the above, a blank line space and then the predicate:
ansi_format(Attr, Format, Args) :-
    ansi_format(current_output, Attr, Format, Args).
*/

/* 

Optionally, the description may be followed by one or more tags. Our tag convention is strongly based on the conventions used by javaDoc. It is adviced to place tags in the order they are described below.

@arg Name Description
Defines the predicate arguments. Each argument has its own @arg tag. The first word is the name of the argument. The remainder of the tag is the description. Arguments declarations normally appear in order used by the predicate.

@param Name Description
This is a synonym for @arg, using the JavaDoc tag name.

@throws Term Description
Error condition. First Prolog term is the error term. Remainder is the description.

@error Error Description
As @throws, but the exception is embedded in error(Error, Context).

@author Name
Author of the module or predicate. Multiple entries are used if there are multiple authors.

@version Version
Version of the module. There is no formal versioning system.

@see Text
Point to related material. Often contains links to predicates or files.

@deprecated Alternative
The predicate or module is deprecated. The description specifies what to use in new code.

@compat Standards and systems
When implementing libraries or externally defined interfaces this tag describes to which standard the interface is compatible.

@copyright Copyright holder
Copyright notice.

@license License conditions
License conditions that apply to the source.

@bug Bug description
Known problems with the interface or implementation.

@tbd Work to be done
Not yet realised behaviour that is enticipated in future versions.
*/

		/*******************************
		 *	          CONFIG	       *
		 *******************************/
