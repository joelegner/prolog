#!/usr/bin/env swipl
%% swift.pl

% DCG rules to generate a simple Swift main function with a loop
swift_program -->
    "func main() {", nl,
    indent(for_loop), nl,
    "}", nl,
    nl,
    "main()".

for_loop -->
    "for i in 1...100 {", nl,
    indent(print_line), nl,
    "    }".

print_line -->
    "print(i)".

nl --> "\n".

indent(Rule) --> "    ", Rule.

:- initialization(main, main).

% Entry point predicate
main(_) :-
    phrase(swift_program, Codes),
    string_codes(String, Codes),
    writeln(String).
