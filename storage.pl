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