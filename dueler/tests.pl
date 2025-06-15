:- module(tests, [test_source_code//0]).

test_source_code -->
    unit_tests,
    integration_tests.

unit_tests --> song_model_tests, set_list_tests.
song_model_tests --> [].
set_list_tests --> [].

integration_tests --> first_use_flow_test, set_list_editing_test.
first_use_flow_test --> [].
set_list_editing_test --> [].
