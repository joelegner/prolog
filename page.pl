% -------------------------
% Compiler directives
% -------------------------
:- discontiguous width/2.
:- discontiguous height/2.

% -------------------------
% Stack definition
% -------------------------

% Stack 'calcs' has three pages.
pages(calcs, [page1, page2, page3]).

% Title and dimensions for the stack.
title(calcs, "Footing Design Calculations").
author(calcs, "Joe Legner, PE, SE").
date(calcs, "April 21, 2025").
width(calcs, 8.5).
height(calcs, 11.0).

% -------------------------
% Page-stack relationship
% -------------------------

% Determine which stack a page belongs to.
page_in_stack(Page, Stack) :-
    pages(Stack, PageList),
    member(Page, PageList).

% -------------------------
% Width definitions
% -------------------------

% Width of a stack
width(Stack, Width) :-
    title(Stack, _),  % guard for stacks
    width_stack(Stack, Width).

width_stack(calcs, 8.5).

% Width of a page inherited from its stack
width(Page, Width) :-
    page_in_stack(Page, Stack),
    width(Stack, Width).

% -------------------------
% Height definitions
% -------------------------

% Height of a stack
height(Stack, Height) :-
    title(Stack, _),
    height_stack(Stack, Height).

height_stack(calcs, 11.0).

% Height of a page inherited from its stack
height(Page, Height) :-
    page_in_stack(Page, Stack),
    height(Stack, Height).

% -------------------------
% Derived properties
% -------------------------

% Area of a page based on inherited dimensions
area(Page, Area) :-
    width(Page, W),
    height(Page, H),
    Area is W * H.

complete(Stack) :- 
    title(Stack, _),
    author(Stack, _),
    date(Stack, _).

print_head(Stack) :- 
    title(Stack, Title),
    author(Stack, Author),
    date(Stack, Date),
    writeln(Title),
    writeln(Author),
    writeln(Date).

print_body(_) :- 
    writeln("Body of calcs goes here").

print(Stack) :-
    complete(Stack), % guard
    print_head(Stack),
    print_body(Stack).
