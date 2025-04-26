% Tree printer

print_tree(Node) :-
    print_tree(Node, 0).

print_tree(Node, Indent) :-
    tab(Indent), write(Node), nl,
    (   children(Node, Children)
    ->  NewIndent is Indent + 4,
        print_children(Children, NewIndent)
    ;   true
    ).

print_children([], _).
print_children([Child|Rest], Indent) :-
    print_tree(Child, Indent),
    print_children(Rest, Indent).

% Define children relationships manually based on your facts

children(book, [frontmatter, mainmatter, backmatter]).
children(frontmatter, [title_page, contents, preface, introduction]).
children(title_page, [title, author, date]).
children(title, []).
children(author, []).
children(date, []).
children(contents, []).
children(preface, []).
children(introduction, []).
children(mainmatter, []).
children(backmatter, []).
