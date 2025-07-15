%% storage.pl
% Axiomatic design of music storage system.

% The customer needs:
cn(cn1, 'Unified system to manage storage of music gear').
cn(cn2, 'Way to search for items by name to find their home').
cn(cn3, 'Assignment of each item to a home').
cn(cn4, 'Labels on containers indicating contents').
cn(cn5, 'Catalog of types of items').
cn(cn6, 'Most common items to be readily accessible').
cn(cn7, 'System to work with everyday activities at the house').
cn(cn8, 'System name that helps to talk about it').
cn(cn9, 'Ways to get things easily').
cn(cn10, 'Ways to put back things easily').
cn(cn11, 'Secure place not at risk of theft').
cn(cn12, 'Minimum use of scarce floor space').

% Customer Needs <--> Functional Requirement
cn_fr(
    [cn1], 
    fr0, 'Make music gear storage easy'
).

cn_fr(
    [cn7, cn10, cn11, cn12],
    fr1, 'Store things outside common traffic areas'
).

cn_fr(
    [cn2, cn3, cn4, cn5, cn6, cn8, cn9, cn10],
    fr2, 'Manage information about stored items'
).

% Functional Requirement <--> Design Paramter
fr_dp(fr0, dp0, 'That Storage System (TSS)').
fr_dp(fr1, dp1, 'Storage areas at Walsingham Way').
fr_dp(fr2, dp2, 'Information system').

% Search utilities
all_cn_ids(IDs) :-
    findall(ID, cn(ID, _), IDs).

all_fr_ids(IDs) :-
    findall(ID, cn_fr(_, ID, _), IDs).

all_dp_ids(IDs) :-
    findall(ID, fr_dp(_, ID, _), IDs).

% Unifies with a list of CN IDs that are not covered by any FR.
unfulfilled_cns(Unfulfilled) :-
    all_cn_ids(AllCNs),
    findall(CN, (cn_fr(CNs, _, _), member(CN, CNs)), CoveredCNsDup),
    sort(CoveredCNsDup, CoveredCNs),
    subtract(AllCNs, CoveredCNs, Unfulfilled).

% Unifies with list of lists of items [[CN], [FR], [DP]].
all_ids(IDs) :-
    all_cn_ids(CN_ids),
    all_fr_ids(FR_ids),
    all_dp_ids(DP_ids),
    IDs = [CN_ids, FR_ids, DP_ids].

/*
NOTE!: Not sure about the following. It feels lidyke I should continue with DPs instead of this.
*/

% HOMES =======================================================================

home(front_room).
home(garage).
home(walsingham_way).
home(living_room).
home(uhaul_storage_unit).
home(big_plastic_cabinet).
home(white_cabinet1).
home(white_cabinet2).
home(orange_spool1).
home(orange_spool2).
home(orange_spool3).
home(master_bedroom).
home(dining_area).
home(joes_nightstand).
home(julies_nightstand).
home(dresser).
home(dresser_drawer_lower_left).
home(dresser_drawer_middle_left).
home(dresser_drawer_top_left).
home(dresser_drawer_lower_right).
home(dresser_drawer_middle_right).
home(dresser_drawer_top_right).
home(dresser_drawer_top_middle).
home(chest_of_drawers).
home(router_table).
home(router_table_top_drawer).
home(router_table_bottom_drawer).
home(power_cords_tote).

home_in_home(front_room, walsingham_way).
home_in_home(garage, walsingham_way).
home_in_home(dining_area, walsingham_way).
home_in_home(power_cords_tote, dining_area).
home_in_home(living_room, walsingham_way).
home_in_home(big_plastic_cabinet, garage).
home_in_home(white_cabinet1, garage).
home_in_home(white_cabinet2, garage).
home_in_home(orange_spool1, big_plastic_cabinet).
home_in_home(orange_spool2, big_plastic_cabinet).
home_in_home(orange_spool3, big_plastic_cabinet).
home_in_home(joes_nightstand, master_bedroom).
home_in_home(julies_nightstand, master_bedroom).
home_in_home(dresser, master_bedroom).
home_in_home(chest_of_drawers, master_bedroom).
home_in_home(dresser_drawer_lower_left, dresser).
home_in_home(dresser_drawer_middle_left, dresser).
home_in_home(dresser_drawer_top_left, dresser).
home_in_home(dresser_drawer_lower_right, dresser).
home_in_home(dresser_drawer_middle_right, dresser).
home_in_home(router_table, master_bedroom).
home_in_home(router_table_top_drawer, router_table).
home_in_home(router_table_bottom_drawer, dresser).

% ITEMS =======================================================================

item(bose_headsphones, 'Bose Quiet Comfort Headphones').
item(clipboard, 'Clipboard').
item(joes_bed, 'Joe\'s bed').
item(julies_bed, 'Julie\'s bed').
item(user_manual, 'User manual').
item(xlr10, 'XLR cable, 10 feet long').
item(xlr20, 'XLR cable, 20 feet long').
item(xlr25, 'XLR cable, 25 feet long').
item(xlr50, 'XLR cable, 50 feet long').
item(xlr6, 'XLR cable, 6 feet long').
item(iec, 'IEC power cable').

% Bulk items are not tracked as unique individuals
bulk_item([xlr6, xlr10, xlr20, xlr25, xlr50, user_manual, clipboard, iec]).

% ITEM HOMES ==================================================================
item_home(bose_headsphones, joes_nightstand).
item_home(clipboard, router_table_top_drawer).
item_home(user_manual, dresser_drawer_lower_left).
item_home(xlr10, orange_spool1).
item_home(xlr20, orange_spool2).
item_home(xlr25, orange_spool2).
item_home(xlr50, orange_spool2).
item_home(xlr6, orange_spool1).
item_home(joes_bed, master_bedroom).
item_home(julies_bed, master_bedroom).
item_home(iec, power_cords_tote).

% Helper predicate to find all items in a home (including sub-homes)
items_in_home(Home, Items) :-
    findall(Item, (
        item_home(Item, DirectHome),
        (DirectHome = Home ; home_path(DirectHome, Home))
    ), Items).

% Helper predicate to check if a home is contained within another home (recursively)
home_path(SubHome, ParentHome) :-
    home_in_home(SubHome, ParentHome).
home_path(SubHome, ParentHome) :-
    home_in_home(SubHome, IntermediateHome),
    home_path(IntermediateHome, ParentHome).

% Helper predicate to get the full path of a home
home_full_path(Home, Path) :-
    home_full_path_acc(Home, [], Path).

home_full_path_acc(Home, Acc, Path) :-
    (   home_in_home(Home, Parent) ->
        home_full_path_acc(Parent, [Home|Acc], Path)
    ;   Path = [Home|Acc]
    ).

% Helper predicate to get indentation for hierarchical display
indent(0, '').
indent(N, Indent) :-
    N > 0,
    N1 is N - 1,
    indent(N1, RestIndent),
    atom_concat('  ', RestIndent, Indent).

% Helper predicate to display a home and its contents at a given level
display_home(Home, Level) :-
    indent(Level, Indent),
    items_in_home(Home, Items),
    length(Items, ItemCount),
    format('~w~w (~w items)~n', [Indent, Home, ItemCount]),
    % Display items in this home
    NextLevel is Level + 1,
    display_items_in_home(Home, NextLevel),
    % Display sub-homes
    display_sub_homes(Home, NextLevel).

% Helper predicate to display items directly in a home
display_items_in_home(Home, Level) :-
    findall(Item, item_home(Item, Home), DirectItems),
    indent(Level, Indent),
    forall(member(Item, DirectItems), (
        (   item(Item, Description) ->
            format('~w- ~w: ~w~n', [Indent, Item, Description])
        ;   format('~w- ~w~n', [Indent, Item])
        )
    )).

% Helper predicate to display sub-homes
display_sub_homes(Home, Level) :-
    findall(SubHome, home_in_home(SubHome, Home), SubHomes),
    forall(member(SubHome, SubHomes), display_home(SubHome, Level)).

% Helper predicate to find top-level homes (homes that are not contained in other homes)
top_level_homes(TopHomes) :-
    findall(Home, (home(Home), \+ home_in_home(Home, _)), TopHomes).

report :-
    writeln('=== THAT STORAGE SYSTEM (TSS) REPORT ==='),
    nl,
    
    % Part 1: Homes and their contents (hierarchical)
    writeln('HOMES AND CONTENTS:'),
    writeln('------------------'),
    top_level_homes(TopHomes),
    forall(member(Home, TopHomes), display_home(Home, 0)),
    nl,
    
    % Part 2: Items and their homes (with full paths)
    writeln('ITEMS AND THEIR LOCATIONS:'),
    writeln('-------------------------'),
    findall(Item, item_home(Item, _), AllItems),
    sort(AllItems, SortedItems),
    forall(member(Item, SortedItems), (
        item_home(Item, DirectHome),
        home_full_path(DirectHome, Path),
        reverse(Path, ReversedPath),
        atomic_list_concat(ReversedPath, ' > ', FullPath),
        (   item(Item, Description) ->
            format('~w: ~w (in ~w)~n', [Item, Description, FullPath])
        ;   format('~w (in ~w)~n', [Item, FullPath])
        )
    )),
    nl,
    
    % Part 3: Summary statistics
    writeln('SUMMARY:'),
    writeln('--------'),
    findall(H, home(H), AllHomes),
    length(AllHomes, HomeCount),
    findall(I, item_home(I, _), AllTrackedItems),
    length(AllTrackedItems, ItemCount),
    format('Total homes: ~w~n', [HomeCount]),
    format('Total tracked items: ~w~n', [ItemCount]),
    nl,
    
    % Part 4: Check for unfulfilled customer needs
    writeln('CUSTOMER NEEDS STATUS:'),
    writeln('---------------------'),
    unfulfilled_cns(Unfulfilled),
    (   Unfulfilled = [] ->
        writeln('All customer needs are addressed by functional requirements.')
    ;   writeln('Unfulfilled customer needs:'),
        forall(member(CN, Unfulfilled), (
            cn(CN, Description),
            format('  ~w: ~w~n', [CN, Description])
        ))
    ),
    nl.