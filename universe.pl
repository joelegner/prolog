% Direct containment relation
container_contained(the_universe, cosmic_structures).
container_contained(cosmic_structures, local_galaxy_group).
container_contained(local_galaxy_group, milky_way_galaxy).
container_contained(milky_way_galaxy, orion_cygnus_arm).
container_contained(orion_cygnus_arm, solar_system).
container_contained(solar_system, planets).
container_contained(planets, inner_planets).
container_contained(inner_planets, earth).
container_contained(inner_planets, mercury).
container_contained(inner_planets, venus).
container_contained(inner_planets, mars).
container_contained(earth, northern_hemisphere).
container_contained(earth, southern_hemisphere).
container_contained(northern_hemisphere, north_america).
container_contained(north_america, united_states_of_america).
container_contained(united_states_of_america, the_east).
container_contained(the_east, the_southeast).
container_contained(the_southeast, florida).
container_contained(florida, hillsborough_county).
container_contained(hillsborough_county, valrico).
container_contained(valrico, walsingham_lot).
container_contained(walsingham_lot, walsingham_house).

% Recursive lookup of all containers of a place
where_is(Place, [Container|Rest]) :-
    container_contained(Container, Place),
    where_is(Container, Rest).
where_is(the_universe, []).  % base case

is_within(Place, Container) :-
    where_is(Place, Containers),
    member(Container, Containers).
