live_one_day --> 
    ['Live One Day'],
    ['Sleep'],
    be_awake.

be_awake -->
    ['Be Awake'],
    ['Rest'],
    do_something.

do_something -->
    ['Do something'],
    work,
    play.

work -->
    ['Work'],
    family_business,
    our_business.

our_business -->
    ['Conduct TPE business'].

family_business -->
    ['Conduct family business'],
    pay_the_bills,
    maintenance.

pay_the_bills -->
    ['Pay the bills'].

maintenance -->
    ['Perform maintenance'],
    cleaning,
    trash_and_recycling,
    repairs.

cleaning --> 
    ['Clean'],
    bathing,
    housekeeping.

bathing -->
    ['Bathe yourself'],
    shower,
    bathtub.

shower -->
    ['Take a shower'].
    
bathtub -->
    ['Take a bath'].

housekeeping -->
    ['Clean the house'],
    cleaning_supplies,
    cleaning_equipment.

cleaning_supplies -->
    ['Use cleaning supplies'].

cleaning_equipment -->
    ['Use cleaning equipment'],
    broom,
    mop,
    vacuum.

broom --> 
    ['Use broom'].

mop -->
    ['Use a mop'].

vacuum -->
    ['Use a vacuum'],
    vehicle_vacuuming,
    home_vacuuming,
    emergency_vacuuming.

vehicle_vacuuming -->
    ['Vacuum out a vehicle'].

home_vacuuming -->
    ['Vacuum the house'],
    floor_vacuuming.

floor_vacuuming -->
    ['Vacuum floors'],
    living_room_floor,
    kitchen_floor,
    hallway_floor,
    bedroom_floor.

living_room_floor -->
    ['Vacuum living room floor'].

kitchen_floor -->
    ['Vacuum kitchen floor'].

hallway_floor -->
    ['Vacuum hallway floor'].

bedroom_floor -->
    ['Vacuum bedroom floor'].

emergency_vacuuming -->
    ['Vacuum up messes'].

trash_and_recycling -->
    ['Take out trash and recycling'].

repairs -->
    ['Make necessary repairs'].

play -->
    ['Play'].