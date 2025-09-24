% passions.pl
% Stoic Passions Knowledge Base
% Based on "The Passions according to the Classical Stoa"
% Source: https://people.wku.edu/jan.garrett/stoipass.htm

% Basic definition of passions
passion_definition('Passions or disorders are agitations of the soul contrary to reason and to nature').

% Four principal genera of passions
principal_genus(lust).
principal_genus(fear).
principal_genus(delight).
principal_genus(distress).

% Basic definitions of the four principal genera
genus_definition(lust, 'disorder related to an apparent future good; belief of prospective good and the subject thinks it advantageous to possess it at once', epithumia, libido).
genus_definition(fear, 'fear of approaching evil or disorder', phobos, metus).
genus_definition(delight, 'disorder arising from presence of apparent good; exuberant transport at having secured some coveted object', hedone, laetitia).
genus_definition(distress, 'disorder originating in distress at present evil; newly formed belief of present evil', lupe, aegritudo).

% Passion classification: passion(Name, Genus, Definition, GreekTerm, LatinTerm)

% I. LUST (DESIRE) varieties
passion(anger, lust, 'lust of punishing the person thought to have inflicted an undeserved injury', orge, ira).
passion(rage, lust, 'anger springing up and suddenly showing itself', thumosis, excandescentia).
passion(hate, lust, 'inveterate anger', menis, odium).
passion(enmity, lust, 'anger watching for an opportunity for revenge', misos, inimicitia).
passion(wrath, lust, 'anger of greater bitterness conceived in innermost heart and soul', none, discordia).
passion(greed, lust, 'insatiable lust for money, distinctions, etc.', spanis, indigentia).
passion(longing, lust, 'lust for beholding someone who is not present', himeros, desiderium).

% II. FEAR varieties
passion(sluggishness, fear, 'fear of ensuing toil', oknos, pigritia).
passion(shame, fear, 'fear of disgrace', aischune, pudor).
passion(fright, fear, 'paralyzing fear which causes paleness, trembling and chattering of teeth', ekplexis, terror).
passion(timidity, fear, 'fear of approaching evil', deima, timor).
passion(consternation, fear, 'fear upsetting the mental balance', none, pavor).
passion(pusillanimity, fear, 'fear following upon the heels of fright', agonia, exanimatio).
passion(bewilderment, fear, 'lust for beholding someone who is not present', thorubos, conturbatio).
passion(faintheartedness, fear, 'lasting fear', none, formido).

% III. DELIGHT varieties
passion(malice, delight, 'delight derived from another\'s evil, which brings no advantage to oneself', epikairekakia, malevolentia).
passion(rapture, delight, 'delight soothing the soul by charm of the sense of hearing', kelesis, delectatio).
passion(ostentation, delight, 'delight shown in outward demeanor and puffing oneself out extravagantly', none, iactatio).

% IV. DISTRESS varieties
passion(envy, distress, 'distress incurred by reason of another\'s prosperity, though it does no harm to the envious person', phthonos, invidentia).
passion(rivalry, distress, 'distress lest another be in possession of an object and one have to go without it oneself', zelos, aemulatio).
passion(jealousy, distress, 'distress from the fact that the thing one has coveted is in the possession of the other person as well as one\'s own', zelotupia, obrectatio).
passion(compassion, distress, 'distress from the wretchedness of another in undeserved suffering', eleos, misericordia).
passion(anxiety, distress, 'oppressive distress', achtheos, angor).
passion(mourning, distress, 'distress arising from an untimely death of a beloved object', none, luctus).
passion(sadness, distress, 'tearful distress', none, maeror).
passion(trouble, distress, 'burdensome distress', odune, aerumna).
passion(grief, distress, 'torturing distress', none, dolor).

% Hierarchical relationships
subtype_of(rage, anger).
subtype_of(hate, anger).
subtype_of(enmity, anger).
subtype_of(wrath, anger).

% Temporal aspects
temporal_orientation(lust, future).
temporal_orientation(fear, future).
temporal_orientation(delight, present).
temporal_orientation(distress, present).

% Valuation aspects (apparent good vs evil)
valuation(lust, apparent_good).
valuation(delight, apparent_good).
valuation(fear, apparent_evil).
valuation(distress, apparent_evil).

% Query predicates for practical use

% Find all passions of a given genus
passions_of_genus(Genus, Passions) :-
    findall(Passion, passion(Passion, Genus, _, _, _), Passions).

% Find the definition of a passion
passion_def(PassionName, Definition) :-
    passion(PassionName, _, Definition, _, _).

% Find Greek and Latin terms for a passion
passion_terms(PassionName, Greek, Latin) :-
    passion(PassionName, _, _, Greek, Latin).

% Find all anger-related passions
anger_related(Passions) :-
    findall(Passion,
    (passion(Passion, lust, _, _, _),
    (Passion = anger ; subtype_of(Passion, anger))),
    Passions).

% Find passions related to others (social aspects)
social_passion(Passion) :-
    passion(Passion, _, Definition, _, _),
    (sub_string(Definition, _, _, _, 'another') ; 
     sub_string(Definition, _, _, _, 'other')).

social_passions(Passions) :-
    findall(Passion, social_passion(Passion), Passions).

% Find future-oriented vs present-oriented passions
future_oriented(Passions) :-
    findall(Passion, 
    (passion(Passion, Genus, _, _, _),
    temporal_orientation(Genus, future)),
    Passions).

present_oriented(Passions) :-
    findall(Passion, 
    (passion(Passion, Genus, _, _, _),
    temporal_orientation(Genus, present)),
    Passions).

% Find passions involving apparent goods vs evils
involves_apparent_good(Passions) :-
    findall(Passion, 
    (passion(Passion, Genus, _, _, _),
    valuation(Genus, apparent_good)),
    Passions).

involves_apparent_evil(Passions) :-
    findall(Passion, 
    (passion(Passion, Genus, _, _, _),
    valuation(Genus, apparent_evil)),
    Passions).

% Additional classifications based on content analysis
physical_manifestation(fright, 'paleness, trembling and chattering of teeth').
physical_manifestation(rapture, 'soothing the soul by charm of the sense of hearing').

intensity_marker(inveterate, hate).
intensity_marker(insatiable, greed).
intensity_marker(paralyzing, fright).
intensity_marker(oppressive, anxiety).
intensity_marker(torturing, grief).

% Good emotions/feelings (eupatheiai) - mentioned as NOT being passions
good_emotion(wish).
good_emotion(caution).
good_emotion(joy).

% Things that are NOT passions according to the Stoa
not_passion(physical_pleasure, 'indifferent value, in accord with nature').
not_passion(physical_pain, 'indifferent value, contrary to nature').
not_passion(eros, 'in the wise person does not include desire for intercourse').
not_passion(preliminary_impressions, 'pre-emotions to which even the wise person is subject').

% Examples of queries you can run:
% ?- passions_of_genus(lust, X).
% ?- passion_def(anger, Def).
% ?- social_passions(X).
% ?- future_oriented(X).
% ?- anger_related(X).
% ?- intensity_marker(insatiable, X).