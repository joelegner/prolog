:- module(data_model, [data_model//0]).

data_model --> 
    song_data_model,
    tag_data_model,
    set_list_data_model,
    artist_data_model,
    user_data_model.

song_data_model --> song_entity, song_fields, song_relationships.
song_entity --> [].
song_fields --> [].
song_relationships --> [].

tag_data_model --> tag_entity, tag_fields.
tag_entity --> [].
tag_fields --> [].

set_list_data_model --> set_list_entity, set_list_fields, set_list_relationships.
set_list_entity --> [].
set_list_fields --> [].
set_list_relationships --> [].

artist_data_model --> artist_entity, artist_fields.
artist_entity --> [].
artist_fields --> [].

user_data_model --> user_entity, user_fields, user_preferences.
user_entity --> [].
user_fields --> [].
user_preferences --> [].
