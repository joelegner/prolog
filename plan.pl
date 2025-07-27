%% plan.pl

% It's another attempt to break down work.

job(clean_gutters).

/*
There are two predicates in the procedure task/1. They say:
#1 Something is a task if it is the job.
#2 Something is a task if it is part of a task.

It is best to wrap it in once/1. Usage example:
?- once(task(setup_ladder)).
true.
*/
task(X) :-                   % task/1 #1
    job(X).
task(X) :-                   % task/1 #2
    part_whole(Part, Whole),
    X = Part,
    Whole = ParentTask,
    task(ParentTask).

leaf_task(Task) :-
    task(Task),
    \+ part_whole(_, Task).  % no subtasks

% ordered_leaves(+Task, -Leaves)
% Collect leaves in part_whole/2 order
ordered_leaves(Task, [Task]) :-
    \+ part_whole(_, Task), !.  % base case: it's a leaf
ordered_leaves(Task, Leaves) :-
    findall(Sub, part_whole(Sub, Task), Subs),  % ordered subtasks
    maplist(ordered_leaves, Subs, LeavesList),
    append(LeavesList, Leaves).

% clean_gutters
part_whole(setup_clean_gutters,   clean_gutters).
part_whole(do_clean_gutters,      clean_gutters).
part_whole(cleanup_clean_gutters, clean_gutters).

% setup_clean_gutters
part_whole(setup_ppe,    setup_clean_gutters).
part_whole(setup_bucket, setup_clean_gutters).
part_whole(setup_ladder, setup_clean_gutters).

% do_clean_gutters
part_whole(clean_southwest_gutter,   do_clean_gutters).
part_whole(clean_northwest_gutter,   do_clean_gutters).
part_whole(clean_northeast_gutter,   do_clean_gutters).
part_whole(clean_north_gutters,      do_clean_gutters).
part_whole(clean_garage_gutter,      do_clean_gutters).
part_whole(clean_east_patio_gutter,  do_clean_gutters).
part_whole(clean_south_patio_gutter, do_clean_gutters).

% cleanup_clean_gutters
part_whole(put_away_ladder, cleanup_clean_gutters).
part_whole(put_away_gloves, cleanup_clean_gutters).
part_whole(put_away_hat,    cleanup_clean_gutters).

% setup_ppe
part_whole(put_on_hat,    setup_ppe).
part_whole(put_on_gloves, setup_ppe).

task(TaskAtom) -->
    ['TODO:'],
    { atomic_list_concat(WordList, ' ', TaskAtom) },
    WordList.

/*
?- make, phrase(task('Clean gutters'), Phrase). 
Phrase = [TODO:,Clean,gutters].

task(TaskAtom, In, Out) :-
    In = [TODO:|Rest1],
    atomic_list_concat(WordList, ' ', TaskAtom),
    append(WordList, Out, Rest1).

phrase(task('Clean gutters'), Phrase).

*/
tasks_ordered_tasks(Tasks, OrderedTasks) :-
    OrderedTasks = Tasks.

print_task_sequence :-
    job(Job),
    ordered_leaves(Job, Leaves),
    maplist(writeln, Leaves).
