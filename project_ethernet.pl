% project_ethernet.pl

% # Things
project(tommy_ethernet).
delivers(tommy_ethernet, ethernet_cable).
cable(ethernet_cable).

specified(Cable) :-
    cable(Cable),
    length_known(Cable),
    writeln("Determine cable type.").

length_known(Cable) :-
    cable(Cable),
    writeln("Determine cable length in feet.").

working(Cable) :-
    cable(Cable),
    installed(Cable),
    tested(Cable).

installed(Cable) :-
    cable(Cable),
    routed(Cable),
    terminated(Cable).

routed(Cable) :-
    % nl,
    % writeln("Route Cable:"),
    cable(Cable),
    purchased(Cable),
    run_in_attic(Cable).

purchased(Cable) :-
    cable(Cable),
    specified(Cable),
    writeln("Buy network cable at Lowes or Home Depot.").
    
tool(Project, Tool) :-
    project(Project),
    write("Get tool: "),
    writeln(Tool).

tools(Project) :-
    tool(Project, ladder),
    tool(Project, lighting),
    tool(Project, drill),
    tool(Project, spade_bit),
    tool(Project, saw),
    tool(Project, screwdriver),
    tool(Project, vacuum).    

hole_cut_in_wall(_) :-
    writeln("Cut hole in wall for cable.").

hole_drilled_in_plate(_) :-
    writeln("Drill hole in top plate of wall for cable.").

run_prepared(Cable) :-
    cable(Cable),
    hole_cut_in_wall(Project),
    hole_drilled_in_plate(Project).

cable_pulled(Cable) :-
    run_prepared(Cable),
    writeln("Pull cable between endpoints.").

run_in_attic(Cable) :-
    cable(Cable),
    delivers(Project, Cable),
    tools(Project),
    ladder_set_up(Project),
    cable_pulled(Cable),
    writeln("Run cable through attic").

ladder_set_up(_) :-
    writeln("Set up ladder.").

ladder_taken_down(_) :-
    writeln("Take down ladder.").

terminated(Cable) :-
    cable(Cable),
    writeln("Terminate cable at router."),
    writeln("Terminate cable at computer.").

tested(Cable) :-
    cable(Cable),
    writeln("Test network connection.").   

tools_put_away(Project) :-
    project(Project),
    ladder_taken_down(Project),
    writeln("Put tools away.").

started(Project) :-
    project(Project),
    write("START project: "),
    writeln(Project).

complete(Project) :-
    project(Project),
    started(Project),
    delivers(Project, Cable),
    working(Cable),
    tools_put_away(Project),
    write("END project: "),
    writeln(Project).