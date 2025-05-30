% main.pl
% Example of a launcher file to compile to executable design using this command:
% swipl -o design -c main.pl
:- use_module(design).
:- initialization(run, main).
