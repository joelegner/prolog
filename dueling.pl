% dueling.pl
% Notes on a piano keyboard
:- use_module(piano).

good(performance) :- 
    good(singing),
    good(playing),
    good(selling).

good(singing) :-
    good(key),
    good(lyrics),
    good(notes),
    good(timing).

good(playing) :-
    good(tempo),
    good(chords),
    good(melodies).

good(selling) :-
    good(setup),
    good(prompts),
    good(spaces),
    good(jokes).
