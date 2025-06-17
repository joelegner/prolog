.PHONY: accounting
accounting:
	swipl -s accounting.pl

.PHONY: act
act:
	swipl -s act.pl

.PHONY: all
all: buddhism goals htmlgen kowalski measurement measurement_no_clpr practice project_ethernet zag zig gma_family length_tests

.PHONY: axiology
axiology:
	swipl -s axiology.pl -g run_tests

.PHONY: axiomatic_design
axiomatic_design:
	swipl -s axiomatic_design.pl -g print_design_report

.PHONY: basics
basics:
	swipl -s basics.pl

.PHONY: book
book:
	swipl -s book.pl

.PHONY: bridge
bridge:
	swipl -s bridge.pl

.PHONY: builder
builder: builder.pl fluffernutters.pl
	swipl -o builder -c builder.pl
	./builder fluffernutters.pl
	open .build/fluffernutters.pl/main.pdf

.PHONY: buddhism
buddhism:
	swipl -s buddhism.pl -g main -t halt

.PHONY: business
business:
	swipl -s business.pl

.PHONY: chords
chords:
	swipl -s chords.pl -g main

.PHONY: clean
clean:
	rm $(DESIGN_EXE)

.PHONY: colors
colors:
	swipl -s colors.pl

.PHONY: crossword
crossword:
	swipl -s crossword.pl

.PHONY: dcg
dcg:
	swipl -s dcg.pl

.PHONY: dcg_calc
dcg_calc:
	swipl -s dcg_calc.pl -g run_tests

.PHONY: decomposition
decomposition: $(DP_PNG) $(FR_PNG) $(DP_FR_PNG)
	@open $(DP_PNG)
	@open $(FR_PNG)
	@open $(DP_FR_PNG)

.PHONY: determine
determine:
	swipl -s determine.pl

.PHONY: dueling
dueling:
	swipl -s dueling.pl

.PHONY: failure_driven_loops
failure_driven_loops:
	swipl -s failure_driven_loops.pl

.PHONY: family
family:
	swipl -s family.pl

.PHONY: files
files:
	swipl -s files.pl -g run

.PHONY: fluffernutters
fluffernutters:
	swipl -s fluffernutters.pl -g print_human_task_list -t halt
	swipl -s fluffernutters.pl -g show_calendar -t halt > fluffernutters.ics


.PHONY: footing
footing:
	swipl -s footing.pl -g run_tests

.PHONY: forall
forall:
	swipl -s forall.pl 

.PHONY: gma
gma:
	swipl -s gma.pl -g main

.PHONY: gma_example
gma_example:
	swipl -s gma_example.pl gma.pl

.PHONY: gma_family
gma_family:
	swipl -s gma_family.pl -g print_valid_family_configs -t halt > gma_family.txt

.PHONY: goals
goals:
	swipl -s goals.pl -g print_sorted_goals -t halt

.PHONY: htmlgen
htmlgen: 
	swipl -s htmlgen.pl -g go -t halt

.PHONY: inheritance
inheritance:
	swipl -s inheritance.pl

.PHONY: inches_from_feet
inches_from_feet:
	swipl -s inches_from_feet.pl

.PHONY: json
json:
	swipl -s json.pl

.PHONY: journey
journey:
	swipl -s journey.pl

.PHONY: kowalski
kowalski:
	swipl -s kowalski.pl -g main -t halt

.PHONY: labels
labels:
	./label_files.sh

.PHONY: language
language:
	swipl -s language.pl -g run_tests

.PHONY: law
law:
	swipl -s law.pl

.PHONY: length
length:
	swipl -s length.pl

.PHONY: length_tests
length_tests:
	swipl -s length.pl -s length_tests.pl -g run_tests -t halt

.PHONY: lists
lists:
	swipl -s lists.pl -g run_tests

.PHONY: live_another_day
live_another_day:
	swipl -s live_another_day.pl

.PHONY: main
main: $(DESIGN_EXE)
	$(DESIGN_EXE)

.PHONY: measurement
measurement:
	swipl -s measurement.pl -g main -t halt

.PHONY: measurement_no_clpr
measurement_no_clpr:
	swipl -s measurement_no_clpr.pl -g main -t halt

.PHONY: page
page:
	swipl -s page.pl

.PHONY: philosophy
philosophy:
	swipl -s philosophy.pl

.PHONY: piano
piano:
	swipl -s piano.pl

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

.PHONY: prologscript
prologscript:
	swipl -s prologscript.pl

.PHONY: project_ethernet
project_ethernet:
	swipl -s project_ethernet.pl -g run -t halt

.PHONY: roguelike
roguelike:
	swipl -s roguelike.pl -g start_game

.PHONY: system
system:
	swipl -s system.pl

.PHONY: task
task:
	swipl -s task.pl

.PHONY: testing_example
testing_example:
	swipl -s testing_example.pl

.PHONY: tree
tree:
	swipl -s tree.pl

.PHONY: twt_setup
twt_setup:
	swipl -s twt_setup.pl

.PHONY: universe
universe:
	swipl -s universe.pl

.PHONY: virtues
virtues:
	swipl -s virtues.pl

.PHONY: zag
zag:
	swipl -s axiomatic_design -g print_zag_templates -t halt | pbcopy
	@echo 'Copied zig templates to clipboard with pbcopy'

.PHONY: zig
zig:
	swipl -s axiomatic_design -g print_zig_templates -t halt | pbcopy
	echo 'Copied zig templates to clipboard with pbcopy'

.PHONY: planning
planning:
	swipl -s planning.pl

.PHONY: structural
structural:
	swipl -s structural.pl

.PHONY: orientation_report
orientation_report:
	swipl -s orientation_report.pl -s orientation_reports -g write_canvas -t halt
	cupsfilter orientation_report.txt > orientation_report.pdf
	open orientation_report.pdf

gantt.txt: howl2026.pl
	swipl -s howl2026.pl -g gantt -t halt > gantt.txt
	cat gantt.txt | pbcopy
	cat gantt.txt

howl2026.ics: howl2026.pl
	swipl -s howl2026.pl -g calendar -t halt > howl2026.ics
	cat howl2026.ics | pbcopy
	cat howl2026.ics

.PHONY: calendar
calendar: howl2026.ics

.PHONY: live
live:
	swipl -s live.pl -g main -t halt

.PHONY: twitch
twitch:
	swipl -s twitch.pl -g main -t halt

.PHONY: floorplan
floorplan: floorplan.pdf
	swipl -s floorplan.pl
	open floorplan.pdf
	
floorplan.pdf: floorplan.ps
	ps2pdf floorplan.ps
