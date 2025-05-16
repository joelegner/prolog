.PHONY: all
all: buddhism goals htmlgen kowalski measurement practice

.PHONY: accounting
accounting:
	swipl -s accounting.pl

.PHONY: act
act:
	swipl -s act.pl

.PHONY: axiomatic_design
axiomatic_design:
	swipl -s axiomatic_design.pl -g print_design_report

.PHONY: book
book:
	swipl -s book.pl

.PHONY: bridge
bridge:
	swipl -s bridge.pl

.PHONY: buddhism
buddhism:
	swipl -s buddhism.pl -g main -t halt

.PHONY: business
business:
	swipl -s business.pl

.PHONY: chords
chords:
	swipl -s chords.pl -g main

.PHONY: colors
colors:
	swipl -s colors.pl

.PHONY: dcg_calc
dcg_calc:
	swipl -s dcg_calc.pl -g run_tests

.PHONY: dueling
dueling:
	swipl -s dueling.pl

.PHONY: failure_driven_loops
failure_driven_loops:
	swipl -s failure_driven_loops.pl

.PHONY: footing
footing:
	swipl -s footing.pl -g run_tests

.PHONY: forall
forall:
	swipl -s forall.pl 

.PHONY: gma
gma:
	swipl -s gma.pl -g main

.PHONY: goals
goals:
	swipl -s goals.pl -g print_sorted_goals -t halt

.PHONY: htmlgen
htmlgen: 
	swipl -s htmlgen.pl -g go -t halt

.PHONY: journey
journey:
	swipl -s journey.pl

.PHONY: kowalski
kowalski:
	swipl -s kowalski.pl -g main -t halt

.PHONY: law
law:
	swipl -s law.pl

.PHONY: lists
lists:
	swipl -s lists.pl -g run_tests

.PHONY: live_another_day
live_another_day:
	swipl -s live_another_day.pl

.PHONY: measurement
measurement:
	swipl -s measurement.pl -g main -t halt

.PHONY: page
page:
	swipl -s page.pl

.PHONY: philosophy
philosophy:
	swipl -s philosophy.pl

.PHONY: piano
piano:
	swipl -s piano.pl

.PHONY: tpe
tpe:
	swipl -s tpe.pl

.PHONY: piano_setup
piano_setup:
	swipl -s piano_setup.pl -g ready_to_play
	# swipl -s piano_setup.pl -g ready_to_play -t halt

.PHONY: physics
physics:
	swipl -s physics.pl

.PHONY: practice
practice:
	swipl -s practice.pl -g main -t halt

.PHONY: probability
probability:
	swipl -s probability.pl

.PHONY: roguelike
roguelike:
	swipl -s roguelike.pl -g start_game

.PHONY: system
system:
	swipl -s system.pl

.PHONY: virtues
virtues:
	swipl -s virtues.pl

.PHONY: family
family:
	swipl -s family.pl

.PHONY: basics
basics:
	swipl -s basics.pl

.PHONY: tree
tree:
	swipl -s tree.pl

.PHONY: zig
zig:
	swipl -s axiomatic_design -g print_zig_templates -t halt | pbcopy
	echo "Copied zig templates to clipboard with pbcopy"

.PHONY: zag
zag:
	swipl -s axiomatic_design -g print_zag_templates -t halt | pbcopy
	@echo "Copied zig templates to clipboard with pbcopy"
	
.PHONY: testing_example
testing_example:
	swipl -s testing_example.pl

.PHONY: axiology
axiology:
	swipl -s axiology.pl -g run_tests

.PHONY: labels
labels:
	./label_files.sh

.PHONY: universe
universe:
	swipl -s universe.pl

.PHONY: files
files:
	swipl -s files.pl -g run

.PHONY: inheritance
inheritance:
	swipl -s inheritance.pl

.PHONY: project_ethernet
project_ethernet:
	swipl -s project_ethernet.pl -g run -t halt

.PHONY: twt_setup
twt_setup:
	swipl -s twt_setup.pl

.PHONY: prologscript
prologscript:
	swipl -s prologscript.pl

.PHONY: json
json:
	swipl -s json.pl

.PHONY: determine
determine:
	swipl -s determine.pl

.PHONY: design
design:
	swipl -s design.pl -g run -t halt

