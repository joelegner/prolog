#!/usr/bin/env swipl
% design_report.pl
%
% Usage: ./design_report.pl
% Reads report.json and writes report.tex and report.pdf using pdflatex

:- use_module(library(http/json)).
:- use_module(library(readutil)).
:- use_module(library(process)).

:- initialization(main, main).

main(_) :-
    InputFile = 'report.json',
    OutputTex = 'report.tex',
    OutputPdf = 'report.pdf',

    (   exists_file(InputFile)
    ->  read_json(InputFile, ReportData),
        generate_latex(ReportData, OutputTex),
        run_pdflatex(OutputTex),
        (   exists_file(OutputPdf)
        ->  format('Report generated: ~w~n', [OutputPdf])
        ;   format('Failed to generate PDF.~n')
        )
    ;   print_usage
    ).

print_usage :-
    format('Usage: ./design_report.pl~n'),
    format('Ensure that report.json is present in the current directory.~n').

read_json(File, Data) :-
    open(File, read, Stream),
    json_read_dict(Stream, Data),
    close(Stream).

generate_latex(Data, File) :-
    open(File, write, Stream),
    write(Stream, '\\documentclass{article}\n'),
    write(Stream, '\\usepackage[utf8]{inputenc}\n'),
    write(Stream, '\\title{Design Report}\n'),
    write(Stream, '\\begin{document}\n'),
    write(Stream, '\\maketitle\n\n'),

    (   _{title: Title} :< Data
    ->  format(Stream, '\\section*{Title}\n~w\n\n', [Title])
    ;   true
    ),

    (   _{author: Author} :< Data
    ->  format(Stream, '\\textbf{Author:} ~w\\\\\n\n', [Author])
    ;   true
    ),

    (   _{sections: Sections} :< Data
    ->  write_sections(Stream, Sections)
    ;   true
    ),

    write(Stream, '\\end{document}\n'),
    close(Stream).

write_sections(_, []).
write_sections(Stream, [Section|Rest]) :-
    (   _{heading: Heading, content: Content} :< Section
    ->  format(Stream, '\\section{~w}\n~w\n\n', [Heading, Content])
    ;   true
    ),
    write_sections(Stream, Rest).

run_pdflatex(TexFile) :-
    process_create(path(pdflatex), ['-interaction=nonstopmode', TexFile], [process(_)]).
