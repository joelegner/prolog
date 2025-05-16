% twt_setup.pl
% Planning for The Wonder Twins dueling setup with key drums.

ready_to_play :- 
    ready_to_play(julie),
    ready_to_play(joe),
    sound_system_ready,
    sound_check_completed,
    writeln("Say 'We are ready to play'").

ready_to_play(julie) :-
    keyboard_ready,
    throne_setup(julie),
    microphone_ready(julie).

ready_to_play(joe) :-
    key_drums_ready,
    throne_setup(joe),
    microphone_ready(joe).

throne_setup(Person) :-
    write("Set up throne for "),
    writeln(Person).

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

keyboard_ready :-
    cables_routed_in_shell,
    keyboard_installed_in_shell,
    keyboard_turned_on,
    keyboard_tested.

key_drums_stand_erected :-
    writeln("Set up key drums stand").

macbook_ready :-
    writeln("Set up Macbook").

key_drums_tested :-
    writeln("Test key drums").

key_drums_ready :-
    key_drums_stand_erected,
    macbook_ready,
    key_drums_tested.

shell_erected :-
    writeln("Set up shell").
    
cables_routed_in_shell :-
    shell_erected,
    writeln("Route cables in shell").

keyboard_installed_in_shell :-
    writeln("Install keyboard in shell").

keyboard_turned_on :-
    writeln("Turn on keyboard power").

keyboard_tested :-
    writeln("Test keyboard for sound").

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

power_strips_set_in_place :-
    writeln("Set power strips in place").

power_strips_connected_to_power :-
    writeln("Plug power strips into building power").

power_system_ready :-
    power_strips_set_in_place,
    power_strips_connected_to_power.

sound_check_completed :-
    writeln("Do a sound check").

sound_system_ready :-
    power_system_ready,
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
