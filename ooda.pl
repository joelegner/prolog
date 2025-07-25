%% ooda.pl
/*
https://thedecisionlab.com/reference-guide/computer-science/the-ooda-loop
*/

% Facts
concept(agility).
concept(circumstances).
concept(fact).
concept(decision).
concept(feedback).
concept(hypothesis).
concept(observation).
concept(ooda_loop).
concept(orientation).
concept(situation).

framework(ooda_loop).

item_repr(john_boyd, 'John Boyd').
item_repr(ooda_loop, 'OODA Loop').

iterative(ooda_loop).

model(ooda_loop).

process(ooda_loop).

system(ooda_loop).

value(agility).

% Relations
invention_inventor(ooda_loop, john_boyd).
system_components(ooda_loop, [observation, orientation, decision_making, behavior]).
system_ability(ooda_loop, late_commitment).
ability_value(late_commitment, agility).
process_steps(ooda_loop, [observe, orient, decide, act]).

input_step(situation, observe).
input_step(information, orient).
input_step(orientation, decide).
input_step(decision, act).
% Every step is informed by feedback
input_step(feedback, observe).
input_step(feedback, orient).
input_step(feedback, decide).
input_step(feedback, act).

step_output(observe, observation).
step_output(orient, orientation).
step_output(decide, decision).
step_output(act, situation).
% Every step produces feedback
step_output(observe, feedback).
step_output(orient, feedback).
step_output(decide, feedback).
step_output(act, feedback).

preceding_succeeding(observe, orient).
preceding_succeeding(orient, decide).
preceding_succeeding(observe, act).
preceding_succeeding(act, observe).

question(observation,   'What is really going on right now?').
question(orientation,   'What is really going on right now?').
question(decision,      'What is the best move I can make?').
question(act,           'How is it going?').

process_actor(ooda_loop, joe).
