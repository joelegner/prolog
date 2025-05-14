% project_ethernet.pl
% This demonstrated planning a project using logic.

run :-
    complete(tommy_ethernet).

% It is a top-down design. 
%
% LEVEL 0 =====================================================================
complete(Project) :-
    project(Project),
    started(Project),
    delivers(Project, Cable),
    working(Cable),
    tools_put_away(Project),
    write("END project: "),
    writeln(Project).

% LEVEL 1 =====================================================================
project(tommy_ethernet).
delivers(tommy_ethernet, ethernet_cable).
cable(ethernet_cable).

started(Project) :-
    project(Project),
    write("START project: "),
    writeln(Project).

specified(Cable) :-
    cable(Cable),
    length_known(Cable),
    writeln("Determine cable type.").

working(Cable) :-
    cable(Cable),
    installed(Cable),
    tested(Cable).

tools_put_away(Project) :-
    project(Project),
    ladder_taken_down(Project),
    writeln("Put tools away.").

% LEVEL 2 =====================================================================
length_known(Cable) :-
    cable(Cable),
    writeln("Determine cable length in feet.").

installed(Cable) :-
    cable(Cable),
    routed(Cable),
    terminated(Cable).

tested(Cable) :-
    cable(Cable),
    writeln("Test network connection.").  

ladder_taken_down(_) :-
    writeln("Take down ladder.").

% LEVEL 3 =====================================================================
routed(Cable) :-
    % nl,
    % writeln("Route Cable:"),
    cable(Cable),
    purchased(Cable),
    run_in_attic(Cable).

terminated(Cable) :-
    cable(Cable),
    writeln("Terminate cable at router."),
    writeln("Terminate cable at computer."). 

% LEVEL 4 =====================================================================
purchased(Cable) :-
    cable(Cable),
    specified(Cable),
    writeln("Buy network cable at Lowes or Home Depot.").

run_in_attic(Cable) :-
    cable(Cable),
    delivers(Project, Cable),
    tools(Project),
    ladder_set_up(Project),
    cable_pulled(Cable),
    writeln("Run cable through attic").

% LEVEL 5 =====================================================================
tools(Project) :-
    tool(Project, ladder),
    tool(Project, lighting),
    tool(Project, drill),
    tool(Project, spade_bit),
    tool(Project, saw),
    tool(Project, screwdriver),
    tool(Project, vacuum).    

ladder_set_up(_) :-
    writeln("Set up ladder.").

cable_pulled(Cable) :-
    run_prepared(Cable),
    writeln("Pull cable between endpoints.").

% LEVEL 6 =====================================================================
tool(Project, Tool) :-
    project(Project),
    write("Get tool: "),
    writeln(Tool).

run_prepared(Cable) :-
    cable(Cable),
    hole_cut_in_wall(Project),
    hole_drilled_in_plate(Project).

% LEVEL 7 =====================================================================
hole_cut_in_wall(_) :-
    writeln("Cut hole in wall for cable.").

hole_drilled_in_plate(_) :-
    writeln("Drill hole in top plate of wall for cable.").
