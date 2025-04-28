.PHONY: all
all: go chords practice kowalski world physics virtues measurement

.PHONY: go
go: 
	swipl -s htmlgen.pl -g go -t halt

.PHONY: chords
chords:
	swipl -s chords.pl -g main

.PHONY: practice
practice:
	swipl -s practice.pl -g main -t halt

.PHONY: kowalski
kowalski:
	swipl -s kowalski.pl -g main -t halt

.PHONY: world
world:
	swipl -s world.pl

.PHONY: page
page:
	swipl -s page.pl

.PHONY: dueling
dueling:
	swipl -s dueling.pl

.PHONY: physics
physics:
	swipl -s physics.pl

.PHONY: virtues
virtues:
	swipl -s virtues.pl

.PHONY: measurement
measurement:
	swipl -s measurement.pl -g main -t halt

.PHONY: footing
footing:
	swipl -s footing.pl -g main -t halt

.PHONY: life
life:
	swipl -s life.pl

.PHONY: buddhism
buddhism:
	swipl -s buddhism.pl -g main -t halt

.PHONY: journey
journey:
	swipl -s journey.pl

.PHONY: roguelike
roguelike:
	swipl -s roguelike.pl -g start_game

.PHONY: piano_setup
piano_setup:
	swipl -s piano_setup.pl -g ready_to_play
	# swipl -s piano_setup.pl -g ready_to_play -t halt

.PHONY: system
system:
	swipl -s system.pl

.PHONY: numbers
numbers:
	swipl -s numbers.pl -g run_tests

.PHONY: philosophy
philosophy:
	swipl -s philosophy.pl

.PHONY: failure_driven_loops
failure_driven_loops:
	swipl -s failure_driven_loops.pl -g write_pet_names -t halt

.PHONY: forall
forall:
	swipl -s forall.pl 

.PHONY: goals
goals:
	swipl -s goals.pl -g print_sorted_goals -t halt

.PHONY: colors
colors:
	swipl -s colors.pl

.PHONY: book
book:
	swipl -s book.pl

.PHONY: gma
gma:
	swipl -s gma.pl -g main

.PHONY: axiomatic_design
axiomatic_design:
	swipl -s axiomatic_design.pl

.PHONY: dcg_calc
dcg_calc:
	swipl -s dcg_calc.pl -g main -t halt

.PHONY: accounting
accounting:
	swipl -s accounting.pl

.PHONY: act
act:
	swipl -s act.pl
