% house.pl
bathroom(kids_bathroom).
bathroom(master_bathroom).

bedroom(back_bedroom).
bedroom(front_bedroom).
bedroom(master_bedroom).

room(back_porch).
room(dining_area).
room(family_room).
room(garage).
room(kids_hallway).
room(kitchen).
room(living_room).
room(master_closet).
room(master_commode).
room(laundry_room).

% Universals
room(X) :- bathroom(X).
room(X) :- bedroom(X).

room_door_room(master_bedroom, pocket_door, master_bathroom).
room_door_room(master_bedroom, door, master_commode).
room_door_room(master_bedroom, folding_door, master_closet).
room_door_room(master_bedroom, door, living_room).
room_door_room(living_room, open, family_room).
room_door_room(family_room, open, kitchen).
room_door_room(living_room, open, kitchen).
room_door_room(living_room, open, dining_area).
room_door_room(living_room, open, kids_hallway).
room_door_room(kids_hallway, door, kids_bathroom).
room_door_room(kids_hallway, door, front_bedroom).
room_door_room(kids_hallway, door, back_bedroom).
room_door_room(kitchen, pocket_door, laundry_room).
room_door_room(kitchen, door, garage).
room_door_exit(garage, overhead_sectional_door).
room_door_exit(master_bedroom, french_doors).
room_door_exit(family_room, french_doors).
room_door_exit(living_room, front_door).

door(D) :-
    room_door_room(_, D, _).
door(D) :-
    room_door_exit(_, D).

/*
We can count the number of bedrooms.

```prolog
?- findall(Room, bedroom(Room), Bedrooms), length(Bedrooms, Number_bedrooms).
Bedrooms = [master_bedroom,front_bedroom,back_bedroom],
Number_bedrooms = 3.
```

And we can do the same for rooms.

```prolog
?- findall(Room, room(Room), Rooms), length(Rooms, Number_rooms).
Rooms = [kitchen,master_bedroom,front_bedroom,back_bedroom],
Number_rooms = 4.
```
*/
