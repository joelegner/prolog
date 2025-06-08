#!/usr/bin/env swipl
% determine.pl
%
% Usage: ./determine.pl report.json
% Reads the given JSON file and template.tex, writes report.tex, and compiles report.pdf using pdflatex

:- use_module(library(http/json)).
:- use_module(library(readutil)).
:- use_module(library(process)).
:- use_module(library(strings)).

:- initialization(main, main).

main(Argv) :-
    ( Argv = [JsonFile] ->
        true
    ; 
        print_usage,
        halt(1)
    ),

    TemplateFile = 'template.tex',
    OutputTex = 'report.tex',
    OutputPdf = 'report.pdf',

    (   exists_file(JsonFile), exists_file(TemplateFile)
    ->  read_json(JsonFile, ReportData),
        read_file_to_string(TemplateFile, Template, []),
        fill_template(Template, ReportData, FinalLatex),
        write_file(OutputTex, FinalLatex),
        run_pdflatex(OutputTex),
        (   exists_file(OutputPdf)
        ->  format('Report generated: ~w~n', [OutputPdf])
        ;   format('Failed to generate PDF.~n')
        )
    ;   format('Error: Required files missing: ~w or ~w~n', [JsonFile, TemplateFile]),
        print_usage,
        halt(1)
    ).

print_usage :-
    format('Usage: ./determine.pl report.json~n'),
    format('Requires: report.json and template.tex in the current directory.~n').

read_json(File, Data) :-
    open(File, read, Stream),
    json_read_dict(Stream, Data),
    close(Stream).

write_file(File, Content) :-
    open(File, write, Stream),
    write(Stream, Content),
    close(Stream).

fill_template(Template, Data, Final) :-
    dict_get(title, Data, '', Title),
    dict_get(author, Data, '', Author),
    dict_get(sections, Data, [], Sections),
    sections_to_latex(Sections, SectionText),
    replace_placeholders(Template, Title, Author, SectionText, Final).

replace_placeholder(Template, Placeholder, Replacement, Result) :-
    split_string(Template, Placeholder, '', Parts),
    atomic_list_concat(Parts, Replacement, Result).

replace_placeholders(Template, Title, Author, Sections, Final) :-
    replace_placeholder(Template, '<<TITLE>>', Title, T1),
    replace_placeholder(T1, '<<AUTHOR>>', Author, T2),
    replace_placeholder(T2, '<<SECTIONS>>', Sections, Final).

sections_to_latex([], '').
sections_to_latex([S|Rest], Latex) :-
    _{heading: H, content: C} :< S,
    format(string(Section), '\\section{~w}\n~w\n\n', [H, C]),
    sections_to_latex(Rest, OtherSections),
    string_concat(Section, OtherSections, Latex).

run_pdflatex(TexFile) :-
    process_create(path(pdflatex), ['-interaction=nonstopmode', TexFile], [process(_)]).

% Helper predicate: get dict value with default
dict_get(Key, Dict, Default, Value) :-
    ( get_dict(Key, Dict, V) -> Value = V ; Value = Default ).
