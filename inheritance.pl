% inheritance.pl
% Classes
% https://book.simply-logical.space/src/text/2_part_ii/4.3.html
instrument(X):-wind(X).
instrument(X):-string(X).
instrument(X):-percussion(X).
wind(X):-woodwind(X).
wind(X):-brass(X).
string(X):-plucked(X).
string(X):-bowed(X).
string(X):-keyboard(X).
percussion(X):-tuned(X).
percussion(X):-untuned(X).

% Instances
woodwind(recorder).
woodwind(flute).
woodwind(oboe).
woodwind(saxophone).
brass(trumpet).
brass(trombone).
brass(horn).
plucked(guitar).
plucked(lute).
plucked(harp).
bowed(violin).
bowed(cello).
keyboard(harpsichord).
keyboard(piano).
tuned(triangle).
tuned(kettledrum).
untuned(cymbal).
untuned(snaredrum).

function(X, musical):-instrument(X).

% Materials
material(flute, metal).
material(saxophone, metal).
material(X, wood):-woodwind(X).
material(X, metal):-brass(X).
material(X, wood):-string(X).
material(X, metal):-percussion(X).

% Actions
action(oboe, reed(double)).
action(saxophone, reed(single)).
action(harpsichord, plucked).
action(piano, hammered).
action(X, reed(lip)):-brass(X).
action(X, plucked):-plucked(X).
action(X, bowed):-bowed(X).
action(X, hammered):-percussion(X).
