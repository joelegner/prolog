system(Sys) :- has_parts(Sys, _), has_purpose(Sys, _).

has_parts(capsule, [electrical_leads]).
has_parts(microphone, [body, windscreen, capsule]).
has_purpose(capsule, transduce_sound_to_electricity).
has_purpose(microphone, transduce_sound_to_electricity).

% ?- system(capsule).
% true.

% ?- system(body).
% false.

% ?- system(microphone).
% true.