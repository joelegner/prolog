#!/usr/bin/env swipl
%% swift.pl
swift_program -->
    "@main", nl,
    "struct Main {", nl,
    indent(main_function), nl,
    "}", nl.

main_function -->
    "static func main() {", nl,
    indent(for_loop), nl,
    "    }".

for_loop -->
    "for i in 1...100 {", nl,
    indent(print_line), nl,
    "    }".

print_line -->
    "print(i)".

nl --> "\n".

indent(Rule) --> "    ", Rule.

:- initialization(main, main).

main(_) :- 
    phrase(swift_program, Codes), string_codes(String, Codes),
    writeln(String).