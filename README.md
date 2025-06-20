# Prolog
Playground for all things Prolog-related. The name Prolog is a shortened version "programming with logic". Prolog is unlike anything I have tried to use before. The more I use it, the more I like it. Already after a few weeks of practice, I am solving some real world problems with it.

There are various implementations of Prolog available. The one assumed here is **SWI-Prolog** which seems to be pronounced by spelling out the SWI part: 's-w-i prolog.' 

- Main website: [https://www.swi-prolog.org](https://www.swi-prolog.org)
- SWI-Prolog Manual: [https://www.swi-prolog.org/pldoc/refman/](https://www.swi-prolog.org/pldoc/refman/)
- SWI-Tinker: [https://wasm.swi-prolog.org/wasm/tinker](https://wasm.swi-prolog.org/wasm/tinker)
- Useful Prolog References: [https://swi-prolog.discourse.group/t/useful-prolog-references/1089](https://swi-prolog.discourse.group/t/useful-prolog-references/1089)
- ECLiPSe Constraint Programming System: [https://eclipseclp.org](https://eclipseclp.org)
- awesome-prolog: [https://github.com/klaudiosinani/awesome-prolog](https://github.com/klaudiosinani/awesome-prolog)

There is another Prolog implementation installed on my computer called **Scryer Prolog**, and that one is used here for `scraping.pl`. The top level command is `scryer-prolog`. 

# HTML Generation

The `htmlgen.pl` file is one of the first in this repository. It is not as relevant now that I understand definite clause grammars (DCGs) a little better. Now that I am a little wiser, I would not do it like this. But I leave it here for future reference.

# Possible DCG Applications

Any of these might be good DCG applications.

- [GraphViz](https://graphviz.org/) – A powerful open-source tool for visualizing structured data as diagrams.
- [HTML](https://developer.mozilla.org/en-US/docs/Web/HTML) – The standard markup language for creating web pages and web applications.
- [LaTeX](https://www.latex-project.org/) – A high-quality typesetting system, often used for technical and scientific documentation.
- [Markdown](https://daringfireball.net/projects/markdown/) – A lightweight markup language with plain text formatting syntax, designed to be easy to write and read.
- [Mermaid](https://mermaid-js.github.io/) – A JavaScript-based diagramming and charting tool that uses a simple markdown-like script language.
- [Plain text](https://en.wikipedia.org/wiki/Plain_text) – Text that contains no formatting other than spaces and line breaks.
- [PlantUML](https://plantuml.com/) – A tool to create UML diagrams from a plain text language.

I am also interested in generating code from DCG rules. This is a strange application of DCGs to be sure. But it seems to be worth continued exploration. The file to look at today for this example is `dueler/app.pl`. It is super primitive, like the scrawl of a toddler. But it is roughly the shape of a human stick figure. 

# Chord Finding

ChatGPT wrote this program at my request. It was an early experiment.

```bash
% make chords
swipl -s chords.pl
Enter chord name (e.g., C, Dm, G7, Fmaj7, Ddim, D+): Dm
Notes in D minor: [D,F,A]
```

# Testing

See `world.pl` for example of testing. It looks like this:

```prolog
% Facts
married(joe, julie).

% Predicate that handles symmetry
are_married(X, Y) :-
    married(X, Y), !; % note the cut operator `!`
    married(Y, X).

% BEGIN TESTS ================================================
:- begin_tests(marriage).

test('Joe is married to Julie') :-
    are_married(joe, julie).

test('Julie is married to Joe') :-
    are_married(julie, joe).

:- end_tests(marriage).
% END TESTS ================================================
```

To run the tests, you need to do the following:

1. Start SWI-Prolog.
2. Consult the file `world.pl`.
3. Use `run_tests.` predicate.

At the command line:

```bash
% swipl -s world.pl
Welcome to SWI-Prolog (threaded, 64 bits, version 9.2.9)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).
```

This starts a Prolog session with the `world.pl` clauses loaded. Now we can start the tests using `run_tests/0` predicate.

```prolog
?- run_tests.
[2/2] marriage:Julie is married to Joe ................. passed (0.000 sec)
% All 2 tests passed in 0.016 seconds (0.012 cpu)
true.
```
