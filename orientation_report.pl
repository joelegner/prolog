% You can edit these lines daily. Each section should produce 5 lines (1 title + 4 or fewer lines padded to 5 total).

market_conditions(RowIn, Col, RowOut) :-
    Lines = [
        'CURRENT MARKET CONDITIONS',
        'AI is entering music in a big way.',
        'Live music might be bigger than songwriting.',
        'Cruise contracts offer most immediate income.',
        'Piano bar offers most stable income.',
        'Windish is advertising for solo piano, crew.'
    ],
    orientation_reports:write_section(RowIn, Col, Lines, RowOut).

opportunities_and_threats(RowIn, Col, RowOut) :-
    Lines = [
        'KEY OPPORTUNITIES AND THREATS',
        '=> Booking cruise contracts with Dezi for 2026.',
        '=> Getting popular in the Lifestyle world.',
        '=> Booking cruise contracts with Dezi for 2026.',
        '=> Getting better work when Pat George retires.',
        'XX Running out of savings.'
    ],
    orientation_reports:write_section(RowIn, Col, Lines, RowOut).

assumptions_and_beliefs(RowIn, Col, RowOut) :-
    Lines = [
        'ASSUMPTIONS AND BELIEFS',
        'Pat George really will retire soon.',
        'A piano bar could be a big hit.',
        'Tommy wants to continue living at Walsingham Way.'
    ],
    orientation_reports:write_section(RowIn, Col, Lines, RowOut).

recent_actions_outcomes(RowIn, Col, RowOut) :-
    Lines = [
        'RECENT ACTIONS AND OUTCOMES',
        'Moved solo show outside to good effect.',
        'Katy Marquart played JF Kicks and did well. She is good enough.',
        'Joe and Julie played Italian Club, and it went well. Not much in Italian.',
        'Joey came to Florida and got to bond with his new girl.',
        'Joe and Julie went thrifting and found costume ideas and show outfits.'
    ],
    orientation_reports:write_section(RowIn, Col, Lines, RowOut).

tactical_priorities(RowIn, Col, RowOut) :-
    Lines = [
        'TACTICAL PRIORITIES',
        'Fitness, weight loss, working out, getting in shape, eating right.',
        'Fix the CR-V.',
        'Prepare for Fluffernutters: bits, costumes, songs, parodies.',
        'Gig at Grand Central on Friday from 7-10pm. Joe plays.',
        'Gig at Made in Italy on Saturday from 9pm-12am. Joe plays.'
    ],
    orientation_reports:write_section(RowIn, Col, Lines, RowOut).

unknowns(RowIn, Col, RowOut) :-
    Lines = [
        'CRITICAL UNKNOWNS',
        'What schedule will Dezi come back with?',
        'What does our 2026 look like?',
        'Is Joey moving back to Florida?',
        'Are we selling either house?',
        'Is anyone moving at all?'
    ],
    orientation_reports:write_section(RowIn, Col, Lines, RowOut).

% Build the page by calling specific report sections from orientation_report.pl
build_page :-
    clear_canvas,
    left_text(1, 'That Piano Entertainment, LLC'),
    right_text(1, 'Weekly Orientation Report'),
    horizontal_line(2),
    right_text(3, 'Sunday    June  8'),
    right_text(4, 'Monday    June  9'),
    right_text(5, 'Tuesday   June 10'),
    right_text(6, 'Wednesday June 11'),
    right_text(7, 'Thursday  June 12'),
    right_text(8, 'Friday    June 13'),
    right_text(9, 'Saturday  June 14'),
    the_stack(4, 1),
    bottom_row(BRow),
    ARow is BRow - 1,
    left_text(ARow, 'https://github.com/joelegner/prolog/blob/main/orientation_report.pl'),
    timestamp(Stamp), 
    right_text(BRow, Stamp).
