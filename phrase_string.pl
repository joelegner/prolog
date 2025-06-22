%% phrase_string.pl
:- module(phrase_string, [
    phrase_string/2,
    phrase_to_stream/2,
    phrase_to_stream/3
]).

%% phrase_to_stream(+DCGBody, +Stream)
%  Writes the DCGBody to the given stream.
phrase_to_stream(DCGBody, Stream) :-
    phrase_to_stream(DCGBody, Stream, []).

%% phrase_to_stream(+DCGBody, +Stream, +Options)
%  Options currently unused, but allows future extensions.
phrase_to_stream(DCGBody, Stream, _Options) :-
    phrase(DCGBody, Codes),
    format(Stream, '~s', [Codes]).

phrase_string(Rule, Phrase) :-
    phrase(Rule, Codes),
    string_codes(Phrase, Codes).
