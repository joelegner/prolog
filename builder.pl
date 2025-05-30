:- use_module(library(filesex)).
:- use_module(library(process)).
:- use_module(library(readutil)).

% Simple fluffernutters.pl data file example:
% fluffernutter_ingredient(banana).

main :-
    current_prolog_flag(argv, [InputFile|_]),
    writeln(InputFile),

    atomic_list_concat(['.build/', InputFile], BuildDir),
    writeln(BuildDir),

    atomic_list_concat([BuildDir, '/images'], ImageDir),
    writeln(ImageDir),    

    writeln("📂 Creating directories..."),
    make_directory_path(BuildDir),
    make_directory_path(ImageDir),

    format("🔍 Consulting ~w~n", [InputFile]),
    consult(InputFile),

    % Verify consult worked by printing ingredient
    fluffernutter_ingredient(Ingredient),
    format("🍞 Fluffernutter ingredient: ~w~n", [Ingredient]),

    format("📄 Creating LaTeX...~n", []),
    create_latex(BuildDir, ImageDir, Ingredient),

    format("📚 Running pdflatex...~n", []),
    run_pdflatex(BuildDir),

    format("✅ Done.~n", []).

% --- Create LaTeX file with substitution
create_latex(BuildDir, _ImageDir, Ingredient) :-
    atomic_list_concat([BuildDir, '/main.tex'], TexPath),
    % Just a placeholder image path (or leave empty if you want)
    RelImagePath = 'images/figure.png',  % relative from .build directory

    % Read the builder.tex template file (must exist in current dir)
    read_file_to_string('builder.tex', Template, []),

    % Replace placeholders
    replace_placeholder(Template, '<<INGREDIENT>>', Ingredient, Tmp1),
    replace_placeholder(Tmp1, '<<IMAGEPATH>>', RelImagePath, Content),

    % Write substituted LaTeX to main.tex
    setup_call_cleanup(
        open(TexPath, write, Out),
        format(Out, "~s", [Content]),
        close(Out)
    ).

% Replace all occurrences of Placeholder in String with Replacement
replace_placeholder(String, Placeholder, Replacement, Result) :-
    split_string(String, Placeholder, Placeholder, Parts),
    atomic_list_concat(Parts, Replacement, Result).

% --- Run pdflatex to produce PDF in BuildDir
run_pdflatex(BuildDir) :-
    MainTex = 'main.tex',
    process_create(path(pdflatex),
        ['-interaction=nonstopmode', MainTex],
        [cwd(BuildDir), process(PID)]),
    process_wait(PID, _Status),

    atomic_list_concat([BuildDir, '/main.pdf'], PdfPath),
    format("📄 PDF created at ~w~n", [PdfPath]).

% Run main on startup
:- initialization(main, main).
