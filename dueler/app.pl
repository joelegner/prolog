:- module(app, [app_source_code//0]).

:- use_module(data_model).      % <-- required because app_source_code uses data_model
:- use_module(user_interface).  % <-- also needed here
:- use_module(helpers).         % <-- same here
:- use_module(struct).         % <-- same here
:- use_module(text_util).         % <-- same here

app_source_code --> 
    the_app,
    data_model,
    user_interface,
    helpers.

the_app --> 
    ['import SwiftUI'], nl,
    ['@main'], nl,
    struct('DuelerMVPApp').
