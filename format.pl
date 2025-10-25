%% format.pl

%% format_examples.pl

format_examples :-
    % ATOMS (~w, ~a)
    Atom = hello_world,
    format('~~w as atom (quotes if needed) = ~w~n', [Atom]),
    format('~~a as atom (raw) = ~a~n', [Atom]),

    % INTEGERS (~d, ~D, ~r, ~R)
    Int = 42,
    format('Decimal (~~d) = ~d~n', [Int]),
    format('Decimal (~~D) = ~D~n', [Int]),
    format('Binary (~~r) = ~r~n', [Int]),
    format('Hex (~~R) = ~R~n', [Int]),

    % FLOATING POINT NUMBERS (~f, ~e, ~g)
    Pi = 3.14159,
    format('Fixed-point (~~2f) = ~2f~n', [Pi]),
    format('Scientific (~~2e) = ~2e~n', [Pi]),
    format('Shortest (~~4g) = ~4g~n', [Pi]),

    % PADDING & WIDTH (~N)
    Life = 42,
    format('Minimum width (~~10d) = ~10d~n', [Life]),

    % TABULATION (~t, ~T)
    format('~tLeft~20tRight~40tEnd~n', []),

    % CHARACTER (~c)
    format('Character (~~c) = ~c~n', [65]),  % ASCII code 65 = 'A'

    % STRING (~s)
    String = "Prolog",
    format('String (~~s) = ~s~n', [String]),

    % PERCENT (~%)
    format('Percent sign = 100%% correct~n', []),

    % NEWLINE (~n)
    format('Line above ends here~nLine starts here~n', []),

    % FILL CHAR (~`)
    format('Fill example: ~`*tHello~20|~n', []).

floats :-
    X is pi,
    Y is pi,
    Z is pi,
    Avogadro is 6.02e23,
    Life is 42,
    % ~f = Floating point
    format('X        = ~2f~n', [X]),
    format('Y        = ~3f~n', [Y]),
    format('Z        = ~4f~n', [Z]),
    format('Avogadro = ~4f~n', [Avogadro]),
    % ~e = Scientific notation
    format('Avogadro = ~2e~n', [Avogadro]),
    % ~g = Shortest representation
    format('Z        = ~4g~n', [Z]),
    format('Avogadro = ~2g~n', [Avogadro]),
    % ~Nd = minimum width
    format('Life     = ~M10d~n', [Life]).

/*
We can do some fancy formatting like this. 

?- simple_statistics.
                               Statistics                               

Runtime: .................... 3.45  Inferences: ................. 60,345
*/
simple_statistics :-
    RunT is 3.45,
    Inf is 60345,
    format('~tStatistics~t~72|~n~n'),
    format('Runtime: ~`.t ~2f~34|  Inferences: ~`.t ~D~72|~n', [RunT, Inf]).