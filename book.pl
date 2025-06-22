% book_dcg.pl

/*
Try this out:
phrase(book, Codes), string_codes(String, Codes), write(String).
*/

book --> 
    preamble,
    begin_document,
    frontmatter,
    mainmatter,
    backmatter,
    end_document.

% Preamble and postamble
preamble -->
    "\\documentclass{book}\n",
    "\\usepackage{makeidx}\n",
    "\\makeindex\n".

begin_document -->
    "\\begin{document}\n".

end_document -->
    "\\end{document}\n".

% Front matter
frontmatter -->
    title_page,
    table_of_contents,
    preface,
    acknowledgements,
    introduction.

title_page -->
    "\\title{My Book Title}\n",
    "\\author{Joe Legner}\n",
    "\\date{}\n",
    "\\maketitle\n".

table_of_contents -->
    "\\tableofcontents\n".

preface -->
    "\\chapter*{Preface}\n",
    "This is the preface.\n".

acknowledgements -->
    "\\chapter*{Acknowledgements}\n",
    "Thanks to all.\n".

introduction -->
    "\\chapter{Introduction}\n",
    "This is the introduction.\n".

% Main matter
mainmatter -->
    part.

part -->
    "\\part{Part Title}\n",
    chapter.

chapter -->
    "\\chapter{Chapter Title}\n",
    section.

section -->
    "\\section{Section Title}\n",
    paragraph.

paragraph -->
    "This is a paragraph.\n".

% Back matter
backmatter -->
    glossary,
    index.

glossary -->
    "\\chapter*{Glossary}\n",
    "\\begin{description}\n",
    "\\item[Term] Definition\n",
    "\\end{description}\n".

index -->
    "\\printindex\n".

/*
This works. It is a good breakthrough.

?- consult('phrase_to_file.pl').
true.

?- phrase_to_file(book, 'book.tex').
true.

?- shell("ls book.tex").
book.tex
true.
*/