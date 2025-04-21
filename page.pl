% There is a page in the world.
page.

% Give it properties.
width(page, 8.5).
height(page, 11.0).

% Give it a calculated property.
area(Page, Area) :-
    width(Page, B),
    height(Page, H),
    Area is B*H.
