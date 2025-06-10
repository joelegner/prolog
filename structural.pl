% structural.pl
% Structural engineering stuff. Why not.
building --> name, superstructure, substructure.
superstructure --> gravity_system, lateral_system.
gravity_system --> floor_system, columns, walls.
floor_system --> slab, beams, trusses, joists.
gravity_walls --> bearing_walls, nonbearing_walls.
lateral_system --> vertical_elements, horizontal_elements.
vertical_elements --> vertical_bracing, shearwalls.
horizontal_elements --> deck_diaphragms, horizontal_bracing.

substructure --> grade_slabs, foundations.
foundations --> shallow_foundations, deep_foundations.
shallow_foundations --> footings, mats.
deep_foundations --> pile_foundations, drilled_shaft_foundations, micropile_foundations.

% Terminal rules for components
name --> [name].
slab --> [slab].
beams --> [beams].
trusses --> [trusses].
joists --> [joists].

columns --> [columns].
walls --> [walls].

bearing_walls --> [bearing_walls].
nonbearing_walls --> [nonbearing_walls].

vertical_bracing --> [vertical_bracing].
shearwalls --> [shearwalls].

deck_diaphragms --> [deck_diaphragms].
horizontal_bracing --> [horizontal_bracing].

grade_slabs --> [grade_slabs].

footings --> [footings].
mats --> [mats].

pile_foundations --> [pile_foundations].
drilled_shaft_foundations --> [drilled_shaft_foundations].
micropile_foundations --> [micropile_foundations].