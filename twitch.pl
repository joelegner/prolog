main :-
    make, phrase(twitch, Phrase),
    maplist(writeln, Phrase).

hr -->
    ['--------------------'].

elaborate -->
    ['(How? This needs elaboration)'].

twitch -->
    ['Make money on Twitch'],
    twitch_account,
    bank_account_connection,
    performance_setup.

twitch_account -->
    ['Manage Twitch account'],
    hr.

bank_account_connection -->
    ['Connect Twitch to bank account'],
    hr.

streamlabs -->
    ['Use Streamlabs'],
    streamlabs_settings,
    streamlabs_window.

streamlabs_settings -->
    ['Adjust StreamLabs settings'],
    hr.

streamlabs_window -->
    ['Use StreamLabs with user interface'],
    streamlabs_sidebar,
    streamlabs_main_window_content.

streamlabs_sidebar -->
    ['Use StreamLabs sidebar'],
    streamlabs_sidebar_editor_menu,
    streamlabs_sidebar_themes_menu,
    streamlabs_sidebar_app_store_menu,
    streamlabs_sidebar_highlighter.

streamlabs_sidebar_editor_menu -->
    ['Open StreamLabs editor sidebar menu'],
    hr.

streamlabs_sidebar_themes_menu --> 
    ['Open StreamLabs themes sidebar menu'],
    hr.

streamlabs_sidebar_app_store_menu -->
    ['Open StreamLabs App Store sidebar menu'],
    hr.

streamlabs_sidebar_highlighter -->
    ['Use StreamLabs highlighter'],
    hr.

streamlabs_main_window_content -->
    ['Work with Streamlabs main window'],
    streamlabs_editor_display,
    streamlabs_mini_feed,
    streamlabs_scene_selector,
    streamlabs_source_selector.

streamlabs_editor_display -->
    ['Use StreamLabs editor display'],
    hr.

streamlabs_mini_feed -->
    ['Use StreamLabs mini feed'], 
    hr.

streamlabs_scene_selector -->
    ['Use StreamLabs scene editor'],
    hr.

streamlabs_source_selector -->
    ['Use StreamLabs source selector'],
    elaborate.


performance_setup -->
    ['Set up our performance system'],
    computer_software,
    room_setup,
    running_state.

computer_software -->
    ['Set up computer software'],
    streamlabs.

room_setup --> 
    ['Physically arrange the room'],
    composition,
    decor,
    equipment.

running_state -->
    ['Get everything up and running for a session'],
    elaborate.

decor -->
    ['Decorate room'],
    hr.

equipment -->
    ['Set up equipment in room'],
    keyboards,
    thrones,
    lighting_equipment,
    sound_equipment,
    computer.

composition -->
    ['Compose the image by arranging the room'],
    keyboard_placement,
    camera_placement,
    microphone_placement,
    sound_placement,
    lighting_placement.

keyboards -->
    ['Set up keyboards in the room'],
    julie_keyboard,
    joe_keyboard.

julie_keyboard -->
    ['Set up Julie\'s keyboard in the room'],
    hr.

joe_keyboard -->
    ['Set up Joe\'s keyboard in the room'],
    hr.

thrones -->
    ['Set up two thrones'],
    joe_throne,
    julie_throne.

joe_throne -->
    ['Set up Joe\'s throne'],
    hr.

julie_throne -->
    ['Set up Julie\'s throne'],
    hr.

lighting_equipment -->
    ['Set up lighting'],
    ring_light,
    broadcast_lights,
    room_lights.

ring_light -->
    ['Set up ring light'],
    hr.

broadcast_lights -->
    ['Set up video lights on stands'], 
    hr.

room_lights -->
    ['Adjust room lighting'],
    room_blinds,
    room_overhead_lights.

room_blinds -->
    ['Adjust room blinds'],
    hr.

room_overhead_lights -->
    ['Adjust overhead light settings'],
    hr.

sound_equipment -->
    ['Set up equipment for sound capture'],
    microphone_system,
    mixer,
    headphones.

microphone_system -->
    ['Set up microphone system'],
    microphone_selection,
    microphone_mounting,
    microphone_connection.

microphone_selection -->
    ['Choose appropriate microphone for the room'],
    hr.

microphone_mounting -->
    ['Mount microphone on stand or boom arm'],
    hr.

microphone_connection -->
    ['Connect microphone to audio interface or mixer'],
    hr.

locate_input_port -->
    ['Find appropriate mic input on interface or mixer'],
    hr.

plug_in_xlr_cable -->
    ['Plug one end of XLR cable into microphone, other into input'],
    hr.

secure_connection -->
    ['Ensure XLR connection is snug and secure'],
    hr.

mixer -->
    ['Set up audio mixer'],
    place_mixer,
    connect_mixer_to_computer,
    adjust_mixer_levels.

place_mixer -->
    ['Place mixer in accessible location'],
    elaborate.

connect_mixer_to_computer -->
    ['Connect mixer output to laptop input'],
    hr.

adjust_mixer_levels -->
    ['Adjust mixer gain and EQ settings'],
    hr.

headphones -->
    ['Set up monitoring headphones'],
    plug_in_headphones,
    check_monitoring_sound.

plug_in_headphones -->
    ['Connect headphones to mixer or interface'],
    hr.

check_monitoring_sound -->
    ['Test headphone output and adjust volume'],
    hr.

computer -->
    ['Set up Julie\'s laptop'],
    hr.

camera_placement -->
    ['Decide on camera placement'],
    hr.

keyboard_placement -->
    ['Decide on keyboard and throne placement'],
    hr.

microphone_placement -->
    ['Decide on microphone placement'],
    hr.

lighting_placement -->
    ['Decide on lighting placement'],
    hr.

sound_placement -->
    ['Decide on sound system placement'],
    hr.
