:- module(piano,
    [
        cable_report/1,
        print_cable_report/0
    ]
).

% Counting cables
% POWER cables
needs_cable(mains1, long_iec_cable).
needs_cable(mains2, long_iec_cable).
needs_cable(lightbar1, long_iec_cable).
needs_cable(monitor1, iec_cable).
needs_cable(monitor2, iec_cable).
needs_cable(mixer1, iec_cable).
needs_cable(power_strip1, extension_cord).
needs_cable(power_strip2, extension_cord).
needs_cable(shell1, power_strip).
needs_cable(shell2, power_strip).
needs_cable(wall_outlet, power_strip).

% SIGNAL cables
needs_cable(piano1, double_instrument_cable).
needs_cable(piano2, double_instrument_cable).
needs_cable(microphone1, xlr_cable).
needs_cable(microphone2, xlr_cable).
needs_cable(mains1, xlr_cable).
needs_cable(mains2, xlr_cable).
needs_cable(monitor1, instrument_cable).
needs_cable(monitor2, instrument_cable).

% Commodities
commodity(iec_cable).
commodity(long_iec_cable).
commodity(extension_cord).
commodity(double_instrument_cable).
commodity(xlr_cable).

% Helper to find all devices for a given Cable
cable_summary(Cable, Count, Devices) :-
    findall(Device, needs_cable(Device, Cable), Devices),
    length(Devices, Count).

% Get unique cable types
unique_cables(Cables) :-
    setof(Cable, Device^needs_cable(Device, Cable), Cables).

% Generate the full report
cable_report(Report) :-
    unique_cables(CableTypes),
    findall([Cable, CountWithSpare, Devices], (
        member(Cable, CableTypes),
        cable_summary(Cable, Count, Devices),
        CountWithSpare is Count + 1
    ), Report).

print_cable_report :-
    cable_report(Report),
    forall(member([Cable, Count, Devices], Report), (
        format('~w, ~d, ~w~n', [Cable, Count, Devices])
    )).