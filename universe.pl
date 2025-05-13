% Direct containment relation
holds(the_universe, cosmic_structures).
holds(cosmic_structures, local_galaxy_group).
holds(local_galaxy_group, milky_way_galaxy).
holds(milky_way_galaxy, orion_cygnus_arm).
holds(orion_cygnus_arm, solar_system).
holds(solar_system, planets).
holds(planets, inner_planets).
holds(inner_planets, earth).
holds(inner_planets, mercury).
holds(inner_planets, venus).
holds(inner_planets, mars).
holds(earth, northern_hemisphere).
holds(earth, southern_hemisphere).
holds(northern_hemisphere, north_america).
holds(north_america, united_states_of_america).
holds(united_states_of_america, the_east).
holds(the_east, the_southeast).
holds(the_southeast, florida).
holds(florida, hillsborough_county).
holds(hillsborough_county, valrico).
holds(valrico, walsingham_lot).
holds(walsingham_lot, walsingham_house).

% Recursive lookup of all containers of a place
where_is(Place, [Container|Rest]) :-
    holds(Container, Place),
    where_is(Container, Rest).
where_is(the_universe, []).  % base case

is_within(Place, Container) :-
    where_is(Place, Containers),
    member(Container, Containers).
