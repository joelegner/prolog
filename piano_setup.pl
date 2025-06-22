gig --> 
    load,
    drive_there,
    set_up,
    sound_check,
    play,
    load_out,
    drive_back,
    unload.

nl --> "\n".

drive_there -->
    "Drive to the venue", nl.

drive_back -->
    "Drive back home", nl.

load -->
    "Put the seats down",
    load_first_shell,
    fill_first_shell,
    load_second_shell,
    fill_second_shell,
    finish_loading.

load_first_shell --> 
    "Load the first shell", nl.

fill_first_shell --> 
    "Fill the first shell", nl.

load_second_shell --> 
    "Load the second shell", nl.

fill_second_shell --> "Fill the second shell", nl.

finish_loading -->
    "Load Yorkvilles",
    "Load dolly",
    "Load keyboards",
    "Load 4bar", nl.

set_up -->
    "Set up the show", nl.

sound_check -->
    "Perform sound check", nl.

play -->
    "Play the show", nl.

load_out -->
    "Load out after the show", nl.

unload -->
    "Unload the car", nl.

run :-
    phrase(gig, Phrase),
    string_codes(String, Phrase), write(String).
