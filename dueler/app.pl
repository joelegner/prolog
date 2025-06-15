:- module(app, [app_source_code//0]).

:- use_module(data_model).      % <-- required because app_source_code uses data_model
:- use_module(user_interface).  % <-- also needed here
:- use_module(helpers).         % <-- same here

app_source_code --> 
    the_app,
    data_model,
    user_interface,
    helpers.

the_app --> 
    app_entry_point,
    app_lifecycle,
    app_navigation.

app_entry_point --> [].
app_lifecycle --> [].
app_navigation --> [].
