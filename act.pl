%% act.pl
%% ACT Core Concepts in Prolog

%% Core processes of ACT
core_process(acceptance).
core_process(defusion).
core_process(self_as_context).
core_process(connect_with_present_moment).
core_process(values).
core_process(committed_action).

%% Triflex processes
triflex(be_present, [connect_with_present_moment, self_as_context]).
triflex(open_up, [acceptance, defusion]).
triflex(do_what_matters, [values, committed_action]).

%% ACT goal and means
act_goal(psychological_flexibility).
means_to_goal(psychological_flexibility, core_process(X)) :- core_process(X).

%% Mindfulness and behavioral processes
mindfulness_process(X) :- member(X, [acceptance, defusion, self_as_context, connect_with_present_moment]).
behavioral_process(X) :- member(X, [values, committed_action]).

%% Values and workability
value_kind(physical_action).
value_kind(psychological_action).

value_direction(compass).

workability(_Behavior, Score) :- number(Score), between(0, 100, Score), Score mod 10 =:= 0.
workable(Behavior) :- workability(Behavior, Score), Score >= 50.
unworkable(Behavior) :- workability(Behavior, Score), Score < 50.

%% Behavior classification
behavior(overt).
behavior(covert).

overt_behavior(X) :- member(X, [eating, drinking, walking, talking, playing_music, reading_silently]).
covert_behavior(X) :- member(X, [thinking, concentrating, focusing, visualizing, mindfulness, imagining, remembering]).

visible_to_camera(X) :- overt_behavior(X).
invisible_to_camera(X) :- covert_behavior(X).

%% Toward and away behaviors
behavior_direction(toward).
behavior_direction(away).

behavior_classification(overt, toward).
behavior_classification(overt, away).
behavior_classification(covert, toward).
behavior_classification(covert, away).

%% Main mantra
mantra([be_present, open_up, do_what_matters]).

%% Quote references
quote(act_core, 'ACT is about taking action.').
quote(be_present, 'Connect with the present moment and the knowing self.').
quote(open_up, 'Accept and unhook.').
quote(do_what_matters, 'Define values and take committed action.').
quote(defusion, 'Watch your thinking.').
quote(acceptance, 'Make space for difficult thoughts and feelings.').
quote(self_as_context, 'Notice yourself noticing.').
quote(flexibility, 'Psychological flexibility leads to a rich and meaningful life.').
quote(toward_move, 'A committed action is a toward move.').

%% Supporting theory
based_on(act, relational_frame_theory).
based_on(act, functional_contextualism).

%% Related systems
related_to(act, buddhism).
related_to(act, rebt).
related_to(act, cbt).

%% Definitions
definition(psychological_flexibility, 'The ability to be present, open up, and do what matters.').
definition(workability, 'A measure of whether a behavior moves you toward your values.').
definition(value, 'Desired quality of action.').
definition(committed_action, 'Effective behavior guided by values.').

/*
% Find all core processes that are mindfulness-based
?- core_process(X), mindfulness_process(X).

% Find all core processes that are behavioral
?- core_process(X), behavioral_process(X).

% Find all triflex groups and their individual core processes
?- triflex(Group, Processes), member(Process, Processes).

% Find overt behaviors visible to camera and classify as toward behaviors
?- overt_behavior(Behavior), visible_to_camera(Behavior), behavior_classification(overt, toward).

% Find covert behaviors invisible to camera and classify as away behaviors
?- covert_behavior(Behavior), invisible_to_camera(Behavior), behavior_classification(covert, away).

% Find behaviors with workability scores on the scale
?- workability(Behavior, Score), between(0, 100, Score), Score mod 10 =:= 0.

% Find all related systems that ACT is connected to
?- related_to(act, System), based_on(act, Theory).

% Find all quotes associated with mindfulness processes
?- mindfulness_process(Process), quote(Process, Quote).
*/