module(twitch, [
    todo/1
]).

% p9 is the project number.
project_name(       p9,     'P9: Joe Twitch Channel Project').
project_author(     p9,     'Joe Legner').
project_end(        p9,     'Make $1 on Twitch').
project_startdate(  p9,     date(2025, 10, 17)).

project_todos(p9, [
    todo('Create a Twitch account if you don\'t already have one.'),
    todo('Research Twitch monetization to understand subscriptions, tips, and ads.'),
    todo('Choose streaming software for Mac (OBS or Streamlabs) and install it.'),
    todo('Set up Twitch streaming integration in the chosen software.'),
    todo('Configure webcam in streaming software.'),
    todo('Configure microphone in streaming software using Scarlett 2i2.'),
    todo('Set up audio monitoring and levels for voice vs. game sound.'),
    todo('Test different resolutions and frame rates for optimal performance.'),
    todo('Position webcam, game window, and overlays to match typical streamer layout.'),
    todo('Set up lighting for professional on-camera appearance.'),
    todo('Organize cables, headphones, keyboard, mouse, and Mac setup for streaming.'),
    todo('Test Angband performance while streaming software is running.'),
    todo('Configure chat overlays and alerts in streaming software.'),
    todo('Assign moderators (Julie, Joey, and optionally other trusted people).'),
    todo('Do a test run of the full stream with Julie and Joey as audience and moderators.'),
    todo('Verify all software (game, streaming, Twitch integration) runs without crashes.'),
    todo('Decide on stream duration (around 1 hour) and prepare a rough outline of gameplay.'),
    todo('Prepare for stream: lighting, webcam, microphone, game ready, desktop organized.'),
    todo('Prepare “Be Right Back” screen for interruptions.'),
    todo('Go live on Twitch for first stream.'),
    todo('Engage with viewers by talking and chatting naturally.'),
    todo('Track chat engagement and viewer feedback during the stream.'),
    todo('After stream, review performance: audio, video, chat, technical issues.'),
    todo('Adjust settings, lighting, overlays, and positioning based on feedback.'),
    todo('Schedule next stream on recurring basis.'),
    todo('Repeat steps 17–25 for each future stream.'),
    todo('Track your own satisfaction and feedback from moderators and viewers after each stream.'),
    todo('Learn from experience how to announce streams or attract new viewers.'),
    todo('Research best practices for Twitch discovery, subscriptions, and donations.'),
    todo('Update production setup or workflow if needed for smoother execution.')
]).

% This generalizes the todo item
project_todo(P, Todo) :-
    project_todos(P, Todos),
    member(Todo, Todos).

project_todos_length(P, N) :-
    project_todos(P, Todos),
    length(Todos, N).

/*
Silly usage example:

?- forall(todo(T), format('- ~w~n', [T])). 
- Create a Twitch account if you don’t already have one.
- Research Twitch monetization to understand subscriptions, tips, and ads.
- Choose streaming software for Mac (OBS or Streamlabs) and install it.
- Set up Twitch streaming integration in the chosen software.
- Configure webcam in streaming software.
- Configure microphone in streaming software using Scarlett 2i2.
- Set up audio monitoring and levels for voice vs. game sound.
- Test different resolutions and frame rates for optimal performance.
- Position webcam, game window, and overlays to match typical streamer layout.
- Set up lighting for professional on-camera appearance.
- Organize cables, headphones, keyboard, mouse, and Mac setup for streaming.
- Test Angband performance while streaming software is running.
- Configure chat overlays and alerts in streaming software.
- Assign moderators (Julie, Joey, and optionally other trusted people).
- Do a test run of the full stream with Julie and Joey as audience and moderators.
- Verify all software (game, streaming, Twitch integration) runs without crashes.
- Decide on stream duration (around 1 hour) and prepare a rough outline of gameplay.
- Prepare for stream: lighting, webcam, microphone, game ready, desktop organized.
- Prepare “Be Right Back” screen for interruptions.
- Go live on Twitch for first stream.
- Engage with viewers by talking and chatting naturally.
- Track chat engagement and viewer feedback during the stream.
- After stream, review performance: audio, video, chat, technical issues.
- Adjust settings, lighting, overlays, and positioning based on feedback.
- Schedule next stream on recurring basis.
- Repeat steps 17–25 for each future stream.
- Track your own satisfaction and feedback from moderators and viewers after each stream.
- Learn from experience how to announce streams or attract new viewers.
- Research best practices for Twitch discovery, subscriptions, and donations.
- Update production setup or workflow if needed for smoother execution.
true.
*/