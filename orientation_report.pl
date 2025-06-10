% You can edit these lines daily. Each section should produce 5 lines (1 title + 4 or fewer lines padded to 5 total).

market_conditions(RowIn, Col, RowOut) :-
    Lines = [
        'CURRENT MARKET CONDITIONS',
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
        'Katy Marquart played JF Kicks and did well. She is good.',
        'Joe and Julie played Italian Club, and it went well.'
    ],
    orientation_reports:write_section(RowIn, Col, Lines, RowOut).

tactical_priorities(RowIn, Col, RowOut) :-
    Lines = [
        'TACTICAL PRIORITIES',
        'Fix the CR-V.',
        'Prepare for Fluffernutters.',
        'Be ready for next weekend\'s three shows.'
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
