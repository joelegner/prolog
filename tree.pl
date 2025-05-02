% tree.pl
% Features of a tree to include:
% - Element:
%   - Trunk     (1 only)
%   - Branch    (1+ sub-elements)
%   - Leaf      (0 sub-elements)
%
% Actions:
% - Traverse the tree
%   - Find element
%     - Find trunk T
%     - Find branch B
%     - Find leaf L
%   - List elements
%     - List trunk [T|[]]
%     - List branches [B|Bs]
%     - List leafs [L|Ls]
%   - Find elements at same level
%   - Find elements above
%   - Find elements below


% First attempt to model a specific tree:
tree(trunk, [elem1, elem2, elem3]).
tree(elem1, [elem1a, elem1b, elem1c]).
tree(elem2, [elem2a, elem2b, elem2c]).

% tree_element(Elem) :-
%     tree(Elem, _).

% tree_element(Elem) :-
%     tree(_, [Elem]).

% children(Elem, Children) :-
%     tree(Elem, Children).

% An element is a leaf if it appears in some Children list
% but is never the first argument of tree/2. Since it is never
% the first argument, it cannot have any subordinate elements.
% That's what makes it a "leaf" in tree terminology.
leaf(Elem) :-
    tree(_, Children),
    member(Elem, Children),
    \+ tree(Elem, _).

% A branch is something that has children
branch(Elem) :-
    tree(Elem, Children),
    Children \= [].

branches(Branches) :-
    findall(Elem, branch(Elem), Branches).

leaves(Leaves) :-
    findall(Elem, leaf(Elem), Leaves).

element(X) :-
    branch(X).
element(X) :-
    leaf(X).