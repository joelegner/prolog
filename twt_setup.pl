% twt_setup.pl
% Planning for The Wonder Twins dueling setup with key drums.

ready_to_play :- 
    sound_system_ready,
    ready_to_play(julie),
    ready_to_play(joe).

ready_to_play(julie) :-
    piano_ready,
    microphone_ready(julie).

microphone_ready(Person) :-
    mic_stand_setup(Person),
    mic_clipped(Person),
    mic_cable_installed(Person).

mic_stand_setup(Person) :-
    write("Set up mic stand for "),
    writeln(Person).

mic_clipped(Person) :-
    write("Clip mic on stand for "),
    writeln(Person).

mic_cable_installed(Person) :-
    write("Connect mic to mixer for "),
    writeln(Person).

piano_ready :-
    shell_erected,
    cables_routed_in_shell,
    piano_installed_in_shell,
    piano_turned_on,
    piano_tested.

shell_erected :-
    writeln("Set up piano shell").
    
cables_routed_in_shell :-
    writeln("Route cables in shell").

piano_installed_in_shell :-
    writeln("Install piano in shell").

piano_turned_on :-
    writeln("Turn on piano power").

piano_tested :-
    writeln("Test piano for sound").

monitors_powered_on :-
    writeln("Power on monitors").

monitors_set_in_place :-
    writeln("Set monitors in place").

monitors_connected :-
    writeln("Connect monitors to power"),
    writeln("Connect monitors to mixer with XLR").

monitors_ready :-
    monitors_set_in_place,
    monitors_connected,
    monitors_powered_on.

sound_system_ready :-
    mixer_ready,
    mains_ready,
    monitors_ready.

mixer_setup :-
    writeln("Set up mixer near joe").

mixer_ready :-
    mixer_setup,
    mixer_powered_on,
    mixer_settings_adjusted.

mixer_power_cable_connected :-
    writeln("Connect mixer to power strip").

mixer_powered_on :-
    mixer_power_cable_connected,
    writeln("Power on mixer").

mixer_settings_adjusted :-
    writeln("Adjust mixer settings").

mains_stand_erected :-
    writeln("Set up mains stands").

mains_on_stand :-
    mains_stand_erected,
    writeln("Put mains on stands").

mains_connected_to_mixer :-
    writeln("Connect mains to mixer with XLR").

mains_ready :-
    mains_on_stand,
    mains_connected_to_mixer,
    writeln("Turn down mains volume on mixer"),
    mixer_powered_on.
