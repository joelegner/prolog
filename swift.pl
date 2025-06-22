#!/usr/bin/env swipl
%% swift.pl

% DCG rules to generate a simple Swift main function with a loop
swift_program(ArgNum) -->
    "func main() {", nl,
    indent(for_loop(ArgNum)), nl,
    "}", nl,
    nl,
    "main()".

for_loop(ArgNum) -->
    { number_codes(ArgNum, Codes) },
    "for i in 1...", Codes, " {", nl,
    indent(print_line), nl,
    "    }".

print_line -->
    "print(i)".

nl --> "\n".

indent(Rule) --> "    ", Rule.

:- initialization(main, main).

% Entry point predicate
main([Arg]) :-
    atom_number(Arg, ArgNum),
    must_be(positive_integer, ArgNum),
    phrase(swift_program(ArgNum), Codes),
    string_codes(String, Codes),
    writeln(String).

% Entry point predicate
main([]) :-
    main(['100']).
