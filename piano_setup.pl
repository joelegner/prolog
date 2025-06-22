gig --> 
    load, nl,
    drive_there, nl,
    set_up, nl,
    sound_check, nl,
    play, nl,
    load_out, nl,
    drive_back, nl,
    unload, nl.

nl --> "\n".

drive_there -->
    "Drive to the venue", nl.

drive_back -->
    "Drive back home", nl.

load -->
    "Create CHECKLIST", nl,
    "Put the seats down", nl,
    load_first_shell, nl,
    fill_first_shell, nl,
    load_second_shell, nl,
    fill_second_shell, nl,
    finish_loading, nl,
    "Complete CHECKLIST", nl.

load_first_shell --> 
    "Load the first shell", nl.

fill_first_shell --> 
    "Fill the first shell", nl.

load_second_shell --> 
    "Load the second shell", nl.

fill_second_shell --> 
    "Fill the second shell", nl.

finish_loading -->
    "Load Yorkvilles", nl,
    "Load keyboards", nl,
    "Load 4bar", nl,
    "Load dolly", nl.

set_up -->
    set_up_pianos, nl,
    set_up_sound, nl,
    set_up_lights, nl,
    turn_on_system, nl.

set_up_pianos -->
    "Set up pianos in shells", nl.

set_up_sound -->
    "Set up entire sound system", nl.

set_up_lights -->
    "Set up the lights", nl.

turn_on_system -->
    "Turn on the mixer", nl,
    "Turn on the keyboards", nl,
    "Turn on the monitors", nl,
    "Turn on the mains", nl,
    "Turn on the lights", nl.

sound_check -->
    sound_check_monitors, nl,
    sound_check_mains, nl.

sound_check_monitors -->
    "Sound check player 1 monitor", nl,
    "Sound check player 2 monitor", nl,
    "Mix vocals and piano", nl,
    "Mix vocals from other player", nl,
    "Mix piano from other player", nl.

sound_check_mains -->
    "Turn up the mains to 9:30", nl,
    "Check the mains are working", nl,
    "Check the volume of the mains", nl,
    "Adjust settings as required", nl,
    "Turn down mains until show", nl.

play -->
    "Turn up the mains", nl,
    "Start the show", nl,
    "Play the show", nl,
    "End the show", nl.

load_out -->
    "Load out after the show", nl.

unload -->
    "Unload the car", nl.

run :-
    phrase(gig, Phrase),
    string_codes(String, Phrase), write(String).
