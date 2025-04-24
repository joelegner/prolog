% There are some real world objects 
% Venue
floor.              % Everything sits on the floor.
power_outlet.       % Source of electrical power.

% PRIMITIVE CONCEPTS
power.
signal.
sound.

% PHYSICAL SETUP atoms
piano_shell.
drum_throne.
keyboard. 
microphone.
microphone_stand.

% SOUND SYSTEM atoms
mains_speaker.      % Powered speaker.
monitor_speaker.    % Powered speaker.


% POWER SYSTEM atoms
power_cable_iec.    % IEC power cable.
power_supply_kb.    % Keyboard power supply with transformer.

% PHYSICAL SYSTEM relations
supports(floor, mains_speaker).         % Floor supports mains
supports(floor, microphone_stand).      % Floor supports mic stand
supports(floor, monitor_speaker).       % Floor supports monitor
supports(floor, piano_shell).           % Floor supports piano shell
supports(microphone_stand, microphone). % Mic stand supports mic
supports(piano_shell, keyboard).        % Shell supports keyboard

% Transitive closure of supports/2
supports_transitively(floor, X) :-
    X \= floor,
    supports(floor, X).
supports_transitively(X, Y) :-
    X \= Y,
    supports(X, Y).
supports_transitively(X, Y) :-
    supports(X, Z),
    X \= Y,  % Prevents trivial cycles
    supports_transitively(Z, Y).

supported(floor).

supported(X) :-
    X \= floor,
    supports_transitively(floor, X).

% POWER SYSTEM Relations
needs(keyboard, power).
needs(mains_speaker, power).
needs(monitor_speaker, power).

% SOUND SYSTEM Relations
source(keyboard, signal).
source(microphone, signal).

% ready_to_play/0 is the over-arching goal. It is the starting goal for when you
% use swipl at the command line. The usage works like this:
%
% swipl -s piano_setup.pl -g ready_to_play -t halt
%
% swipl: The Prolog interpreter we are using.
% -s piano_setup.pl: Consults the file 'piano_setup.pl'.
% -g ready_to_play: Invokes this goal to get things started.
% -t halt: Stops the interpreter after the goal is met. Remove to interact.
ready_to_play :- 
    writeln("Ready to play!").
