% book.pl

book --> 
    frontmatter,
    mainmatter,
    backmatter.

frontmatter --> 
    [title_page],
    [table_of_contents],
    [preface],
    [acknowledgements],
    [introduction].

mainmatter --> 
    part.

part --> 
    [part],
    chapter.

chapter -->
    [chapter],
    section.

section -->
    [section],
    paragraph.

paragraph -->
    [paragraph].

backmatter -->
    [glossary],
    [index].