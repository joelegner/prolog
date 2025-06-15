
main :-
    make, phrase(twitch, Phrase),
    maplist(writeln, Phrase).

twitch -->
    ['Make money on Twitch'],
    twitch_account,
    bank_account_connection,
    performance_setup.

twitch_account -->
    ['Manage Twitch account'].

bank_account_connection -->
    ['Connect Twitch to bank account'].

streamlabs -->
    ['Use Streamlabs'],
    streamlabs_settings,
    streamlabs_window.

streamlabs_settings -->
    ['Adjust StreamLabs settings'].

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
    ['Open StreamLabs editor sidebar menu'].

streamlabs_sidebar_themes_menu --> 
    ['Open StreamLabs themes sidebar menu'].

streamlabs_sidebar_app_store_menu -->
    ['Open StreamLabs App Store sidebar menu'].

streamlabs_sidebar_highlighter -->
    ['Use StreamLabs highlighter'].

streamlabs_main_window_content -->
    ['Work with Streamlabs main window'],
    streamlabs_editor_display,
    streamlabs_mini_feed,
    streamlabs_scene_selector,
    streamlabs_source_selector.

streamlabs_editor_display -->
    ['Use StreamLabs editor display'].

streamlabs_mini_feed -->
    ['Use StreamLabs mini feed'].

streamlabs_scene_selector -->
    ['Use StreamLabs scene editor'].

streamlabs_source_selector -->
    ['Use StreamLabs source selector'].


performance_setup -->
    ['Set up our performance system'],
    computer_software,
    room_setup.

computer_software -->
    ['Set up computer'],
    streamlabs.

room_setup --> 
    ['Physically arrange the room'],
    composition,
    decor,
    equipment.

decor -->
    ['Decorate room'].

equipment -->
    ['Set up equipment in room'],
    keyboards,
    thrones,
    lighting_equipment,
    sound_equipment,
    computer.

composition -->
    ['Compose the image by arranging the room'],
    camera_placement,
    keyboard_placement,
    microphone_placement,
    lighting_placement,
    sound_placement.

keyboards -->
    ['Set up keyboards in the room'].

thrones -->
    ['Set up two thrones'].

lighting_equipment -->
    ['Set up some lighting'].

sound_equipment -->
    ['Set up equipment for sound capture'].

computer -->
    ['Set up Julie\'s laptop'].

camera_placement -->
    ['Decide on camera placement'].

keyboard_placement -->
    ['Decide on keyboard and throne placement'].

microphone_placement -->
    ['Decide on microphone placement'].

lighting_placement -->
    ['Decide on lighting placement'].

sound_placement -->
    ['Decide on sound system placement'].
