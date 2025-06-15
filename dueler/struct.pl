% struct.pl
% Represents a Swift struct
:- module(struct, [struct//1]).

:- use_module(text_util).

struct(Name) -->
    ['struct'],
    [Name],
    [:].
