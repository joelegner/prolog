%% location_event_structure.pl

/*
# The Location Event-Structure (LES) Metaphor

Source: https://people.wku.edu/jan.garrett/302/folkmeta.htm

1.  States are locations (interiors of bounded regions in space). 
    Example: "She's in love."

2.  Changes are movements (into or out of bounded regions). 
    Example: "He went from sad to happy."

3.  Causes are forces. 
    Example: "High winds accelerated the fire."

4.  Causation is forced movement (from one location to another). 
    Example: "The scandal drove the minister from office."

5.  Actions are self-propelled movements. 
    Example: "She moved ahead with her plan."

6.  Purposes are destinations. 
    Example: "He's headed for success."

7.  Means are paths (to destinations). 
    Example: "Education is the path to freedom."

8.  Difficulties are impediments to motion. 
    Example: "We hit a roadblock in negotiations."

9.  Freedom of action is lack of impediments to motion. 
    Example: "Now that the restrictions are lifted, we're free to move forward."

10. External events are large moving objects (that exert force). 
    Example: "The wave of AI has disrupted many companies' plans."

11. Long-term purposeful activities are journeys. 
    Example: "Her career took her from co-op to vice president."
*/

% LES metaphoric mappings
location(X)                      :- state(X).
movement(X)                      :- change(X).
force(X)                         :- cause(X).
forced_movement(X)               :- causation(X).
self_propelled_movement(X)       :- action(X).
destination(X)                   :- purpose(X).
path(X)                          :- means(X).
impediment_to_motion(X)          :- difficulty(X).
lack_of_impediments_to_motion(X) :- freedom_of_action(X).
large_moving_object(X)           :- external_event(X).
journey(X)                       :- long_term_purposeful_activity(X).

/* Example facts */
state(resting).
state(sleeping).
change(waking_up).
cause(gravity).
causation(pushing_box).
action(walking).
purpose(homecoming).
means(road).
difficulty(traffic_jam).
freedom_of_action(no_restrictions).
external_event(wave_of_ai).
long_term_purposeful_activity(career_advancement).
