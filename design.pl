:- module(design, [run/0]).

:- dynamic input_approved/1.
:- dynamic output_approved/1.

% --- Sample Input ---

design_input(footing, [load(100), width(1.0), length(1.0), soil_bearing_capacity(150)]).

% --- Design Logic ---

% Declare the conditions under which a design is acceptable
acceptable_design(Name, Output) :-
    design_input(Name, Input),
    input_approved(Input),
    design_output(Input, Output),
    output_approved(Output),
    code_compliant(Input),
    code_compliant(Output).

% Output of design from input (placeholder)
design_output(Input, Output) :-
    Output = [result(safe), dimensions(Input)].

% Placeholder code compliance check
code_compliant(_).

% --- Run Automatically ---

run :-
    Design = footing,
    design_input(Design, Input),
    assertz(input_approved(Input)),
    design_output(Input, Output),
    assertz(output_approved(Output)),
    (   acceptable_design(Design, FinalOutput)
    ->  format('Design "~w" is acceptable.~n', [Design]),
        format('Output: ~w~n', [FinalOutput])
    ;   format('Design "~w" is NOT acceptable.~n', [Design]),
        halt(1)
    ),
    halt(0).
