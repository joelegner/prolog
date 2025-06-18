% scraping.pl
% Note! Use scryer-prolog.

% It demonstrates a surprising fact: you can have a working Prolog program without
% having to define even a single predicate. Here is one that can extract the titles
% of articles from the Hacker News website and defines zero predicates of its own.

% https://www.youtube.com/watch?v=zKrw-pRI9ac

:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(sgml)). % for HTML parsing
:- use_module(library(xpath)). % for DOM selections

/*
Usage example 1:
    Get the first 20 characters from the stream S.

scryer-prolog scraping.pl
?- http_open("https://news.ycombinator.com", S, []),
length(Cs, 20),
maplist(get_char(S), Cs).
   S = '$stream'(0x7fe2c5105be8), Cs = "<html lang=\"en\" op= ...".
*/

/*
Usage example 2:
    Parse an HTML file from a stream.

?- http_open('https://news.ycombinator.com', S, []), load_html(stream(S), DOM, []).
   S = '$stream'(0x7fe9901146f8), DOM = [element(html,[lang="en",op="news"],[element(head,[],[element(meta,[name="referrer",content="origin"],[]),element(meta,[name="viewport",content="width=device- ..."],[]),element(link,[rel="stylesheet",type="text/css",href="news.css?kc ..."],[]),"\n        ",element(link,[rel="icon",href="y18.svg"],[]),"\n            ...",element(link,[rel="alternate ...",type="applicat ...",title="RSS",href="rss"],[]),"\n        ",element(title,[],["Hacker N ..."])]),element(body,[],[element(center,[],[element(table,[id="hnmain",border="0",cellpadding="0",cellspacing="0",width="85%",bgcolor="#f6f6ef"],["\n        ",element(tbody,[],[element(tr,[],[element(td,[bgcolor="#ff6600"],[element(table,[border="0",cellpadding="0",cellspacing="0",width="100%",style="padd ..."],[element(tbody,[],[element(tr,[],[element(td,[style="width ..."],[element(a,[href="http ..."],[element(img,[src="y18 ...",width="18 ...",height="1 ...",style="border:1px white solid; display:block"],[])])]),"\n      ...",element(td,[style="lin ..."],[element(span,[class="pa ..."],[element(b,[class="h ..."],[element(a,[href="news"],["H ..."])]),"\n  ...",element(a,[... = ...],["new"])," | "|...])]),element(td,[style="te ..."],[element(span,[class="p ..."],["\n  ...",element(a,[... = ...],["login"]),"\n                          "])]),"\n   ..."])])])])]),"\n",element(tr,[id="pagespac ...",title=[],style="height ..."],[]),element(tr,[],[element(td,[],[element(table,[border="0",cellpadding="0",cellspacing="0"],["\n      ...",element(tbody,[],[element(tr,[class="at ...",id="4 ..."],["\n   ...",element(td,[align="right",... = ...|...],[element(span,[... = ...],["1."])]),"  ...",element(...,...,...)|...]),element(tr,[],[element(td,[colspan="2"],[]),element(td,[... = ...],[element(...,...,...)|...])]),"\n  ...",element(tr,[... = ...|...],[]),"\n                "|...])]),"\n"])]),"\n",element(tr,[],[element(td,[],[element(img,[src="s.g ...",height="10 ...",width="0 ..."],[]),element(table,[width="10 ...",cellspacing="0 ...",cellpadding="1"],[element(tbody,[],[element(tr,[],[element(td,[... = ...],[])])])]),element(br,[],[]),"\n",element(center,[],[element(...,...,...)]),element(...,...,...)|...])]),"       ..."])])]),"\n      ",element(script,[type="text/javascr ...",src="hn.js?kc7ZU ..."],[]),"\n  \n"])])].
*/

/* 
Usage example 3:
    Extract title from the DOM.

?- http_open('https://news.ycombinator.com', S, []),
load_html(stream(S), DOM, []),
xpath(DOM, //span(@class="titleline", text), E).
*/
