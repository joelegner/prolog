%% project_management.pl
% pm = Project Manager

name(pm, 'Project Manager').

manages(pm, scope).
manages(pm, schedule).
manages(pm, budget).
manages(pm, quality).
manages(pm, resources).
manages(pm, risk).

/*
Example usage:

?- findall(X, manages(pm, X), Manages).
Manages = [scope,schedule,budget,quality,resources,risk].
*/
