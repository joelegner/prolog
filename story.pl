%% story.pl

storytelling -->
    idea,
    emotion,
    story.

story -->
    exposition,
    rising_action,
    climax,
    falling_action,
    resolution.

exposition -->
    [exposition].

rising_action -->
    [rising_action].

climax -->
    [climax].

falling_action -->
    [falling_action].

resolution -->
    [resolution].

tell_a_story :-
    phrase(story, MyStory),
    writeln(MyStory).