% Your ACT tree as a single compound term
act_tree(
    practice_act(
        be_present(
            connect_with_present_moment,
            connect_with_knowing_self
        ),
        open_up(
            accept,
            defuse
        ),
        do_what_matters(
            identify_values,
            take_committed_action
        )
    )
).

% Human-readable names
name(practice_act, 'Practice ACT').
name(be_present, 'Be Present').
name(open_up, 'Open Up').
name(do_what_matters, 'Do what Matters').
name(connect_with_present_moment, 'Connect with the Present Moment').
name(connect_with_knowing_self, 'Connect with the Knowing Self').
name(accept, 'Accept').
name(defuse, 'Defuse').
name(identify_values, 'Identify Values').
name(take_committed_action, 'Take Committed Action').

% Print tree
print_act_tree :-
    act_tree(Tree),
    print_bullet(Tree, 0).

print_bullet(Term, Indent) :-
    Term =.. [Functor | Args],
    name(Functor, Name),
    print_indent(Indent),
    format('- ~w~n', [Name]),
    NextIndent is Indent + 1,
    maplist({NextIndent}/[Arg]>>print_bullet(Arg, NextIndent), Args).

print_indent(N) :-
    Spaces is N * 2,
    tab(Spaces).

/*
Usage example:

?- print_act_tree.
- Practice ACT
  - Be Present
    - Connect with the Present Moment
    - Connect with the Knowing Self
  - Open Up
    - Accept
    - Defuse
  - Do what Matters
    - Identify Values
    - Take Committed Action
true.
*/