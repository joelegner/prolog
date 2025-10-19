% -----------------------------------------------------------------
% File:  <your_filename>.pl
% Author: <Your Name>
% Version: <version or date>
% Description:
%   <Short summary of the purpose of this file / module.>
%   <High-level overview of key data structures, algorithms, 
%    interactions with other modules, etc.>
%
% Revision history:
%   <date> : <what changed>
%   ...
% License: <license or copyright>
% -----------------------------------------------------------------

/*
   NOTES / design decisions for this file:

   - Modes, determinism and types: decide on a documentation convention 
     (e.g. +, -, ?, =, : etc.) and apply it consistently.
   - Naming conventions: underscores vs intercaps; auxiliary predicates suffixes; 
     single vs plural, etc.
   - Module interfaces: which predicates are exported/public; which stay internal.
   - Partition into sections if the file has multiple logical parts.
   - Use of dynamic predicates, meta-predicates, cuts: annotate carefully.
   - Testing / debugging infrastructure: where to put tests, how to invoke them.
*/

% -----------------------------------------------------------------
% Module (if using a module system)
% -----------------------------------------------------------------

%:- module(<module_name>, [
%    <exported_pred1>/arity1,
%    <exported_pred2>/arity2,
%    …
% ]).

/*
  Explanation of module:
  - What the module provides (main functionality)
  - How it interacts with other modules
  - Key invariants, global state, side-effects (if any)
  - Naming conventions / style decisions specific to this module
*/

% -----------------------------------------------------------------
% Include / use / import directives
% -----------------------------------------------------------------

%:- use_module(library(...)).
%:- use_module(other_module, [pred_a/arity, pred_b/arity]).
%:- meta_predicate(...).     % if you have meta-predicates

/*
  Explanation of imports:
  - Why these are needed
  - Any “aliasing” or renaming decisions
  - Any constraints or expectations on those imported predicates
*/

% -----------------------------------------------------------------
% PUBLIC predicates
% -----------------------------------------------------------------

%% <pred_name>(<mode1> Var1, <mode2> Var2, …) is <determinism>
%  
% <Description line 1>
% <Description line 2>
% ...
%
% <More discussion: side-effects, error behavior, relation to other preds>
%
% Example:
%   ?- <pred_name>(...).  
%
% Throws:
%   <if any errors or exceptional conditions>
%
% Remarks / Caveats:
%   <if special constraints or caveats>
%
<pred_name>(Var1, Var2, …) :-
    % <subgoal 1 — comment explaining role>
    % <subgoal 2 — comment explaining role>
    % …
    % Possibly use cut, if-then-else, etc. — explain carefully
    .

%% <another_pred>(...) is <determinism>
% ...
<another_pred>(...) :-
    % ...
    .

/*
  If needed, group related predicates into subsections with a section‐level comment.
  For example:

  = Section: input processing =

  = Section: core algorithm =

  = Section: output / formatting / I/O =
*/

% -----------------------------------------------------------------
% INTERNAL / auxiliary predicates
% -----------------------------------------------------------------

% (You may choose not to document every auxiliary predicate if they are obvious,
% but if nontrivial, include short “%” comments.)

aux_predicate1(...) :-
    % <comment: what this does in support of public preds>
    .

aux_predicate2(...) :-
    % ...
    .

% -----------------------------------------------------------------
% Error handling predicates (if any)
% -----------------------------------------------------------------

% tbd(Error) etc.

% -----------------------------------------------------------------
% Unit tests / self‐test (optional)
% -----------------------------------------------------------------

/*
   This section defines tests that can be run to check correctness.
   For example, a predicate run_tests/0 that calls test predicates.
*/

%:- begin_tests(<module_name>).

%test(test_name) :-
%    <goal>,
%    assertion(<condition>).

%:- end_tests(<module_name>).

% Alternatively, use a directive that runs tests when file is loaded in a development setting.

% -----------------------------------------------------------------
% End of file
% -----------------------------------------------------------------