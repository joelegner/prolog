%% guitars.pl
% This one is a tongue-in-cheek example of a rule. 
:- module(guitars, [num_guitars_owned_needed/2]).

% Relates number of guitars owned to number ow 
num_guitars_owned_needed(Owned, Needed) :-
    integer(Owned),
    Owned > 0,
    Needed is Owned + 1.

num_guitars_owned_needed(Owned, Needed) :-
    integer(Needed),
    Needed > 1,
    Owned is Needed - 1.

main :-
    write('How many guitars do you have now? '), flush_output(current_output),
    read_line_to_string(user_input, Input),
    (   catch(number_string(N, Input), _, fail),
        integer(N),
        N > 0
    ->  guitars:num_guitars_owned_needed(N, Needed),
        format('You need ~w guitars.~n', [Needed])
    ;   write('Invalid input. Terminating.'), nl,
        halt(1)
    ).

:- initialization(main, main).
