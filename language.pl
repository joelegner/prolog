:- module(ontology, [is_a/2, has_property/2]).

% ----- Ontology Base Facts -----

% Taxonomy (with properties)
is_a_with_property(object, thing, graspable).
is_a_with_property(physical_object, object, mass).
is_a_with_property(organism, physical_object, alive).
is_a_with_property(animal, organism, genetic_material).
is_a_with_property(animal, organism, motile).
is_a_with_property(animal, organism, multicellular).
is_a_with_property(animal, organism, heterotrophic).
is_a_with_property(animal, organism, eukaryotic).
is_a_with_property(animal, organism, reproduces).
is_a_with_property(animal, organism, grows).
is_a_with_property(animal, organism, responds_to_stimuli).
is_a_with_property(animal, organism, has_senses).
is_a_with_property(animal, organism, breathes).
is_a_with_property(animal, organism, has_nervous_system).
is_a_with_property(animal, organism, has_cells_without_cell_wall).
is_a_with_property(animal, organism, consumes_organic_matter).
is_a_with_property(animal, organism, internal_digestion).
is_a_with_property(animal, organism, has_symmetry).
is_a_with_property(animal, organism, has_tissues).
is_a_with_property(animal, organism, capable_of_locomotion).
is_a_with_property(animal, organism, requires_oxygen).
is_a_with_property(animal, organism, has_developmental_stages).
is_a_with_property(animal, organism, has_behavior).

is_a_with_property(plant, organism, photosynthetic).
is_a_with_property(mammal, animal, warm_blooded).
is_a_with_property(bat, mammal, flies).
is_a_with_property(fruit_bat, bat, eats_plants).
is_a_with_property(insectivorous_bat, bat, eats_insects).
is_a_with_property(human, mammal, self_aware).
is_a_with_property(human, physical_object, upright).
is_a_with_property(human, mammal, bipedal).
is_a_with_property(bat, mammal, quadripedal).
is_a_with_property(bat, mammal, wings).
is_a_with_property(insect, animal, six_legs).

% ----- Derived Relations -----

% Immediate parent
is_a(Child, Parent) :-
    is_a_with_property(Child, Parent, _).

% Transitive parent
is_a(Child, Ancestor) :-
    is_a_with_property(Child, Parent, _),
    is_a(Parent, Ancestor).

% Direct property
has_property(Child, Property) :-
    is_a_with_property(Child, _, Property).

% Inherited property
has_property(Child, Property) :-
    is_a_with_property(Child, Parent, _),
    has_property(Parent, Property).

% ----- Tests -----
:- begin_tests(ontology).

test(is_a_direct) :-
    once(is_a(object, thing)),
    once(is_a(physical_object, object)),
    once(is_a(human, mammal)).

test(is_a_transitive) :-
    once(is_a(human, object)),
    once(is_a(human, thing)).

test(has_property_direct) :-
    once(has_property(object, graspable)),
    once(has_property(mammal, warm_blooded)),
    once(has_property(human, self_aware)).

test(has_property_inherited) :-
    once(has_property(human, mass)),
    once(has_property(human, graspable)),
    once(has_property(human, alive)),
    once(has_property(human, heterotrophic)).

:- end_tests(ontology).
