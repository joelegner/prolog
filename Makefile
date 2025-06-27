.PHONY: all
all: goals length_tests measurement measurement_no_clpr zag zig

.PHONY: ansi_term
ansi_term:
	swipl -s ansi_term.pl -g main -g halt

.PHONY: ascii_canvas
ascii_canvas:
	swipl -s ascii_canvas.pl

.PHONY: axiology
axiology:
	swipl -s axiology.pl -g run_tests

.PHONY: axiomatic_design
axiomatic_design:
	swipl -s axiomatic_design.pl -g print_design_report

.PHONY: book
book:
	swipl -s book.pl

.PHONY: business
business:
	swipl -s business.pl

.PHONY: calendar
calendar: howl2026.ics

.PHONY: chords
chords:
	swipl -s chords.pl -g main

.PHONY: clean
clean:
	rm $(DESIGN_EXE)

.PHONY: clipz
clipz:
	scryer-prolog clpz.pl

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

.PHONY: floorplan
floorplan: floorplan.pdf
	open floorplan.pdf

floorplan.pdf: floorplan.ps
	ps2pdf floorplan.ps

floorplan.ps: floorplan.pl
	swipl -s floorplan.pl -g run -t halt

.PHONY: footing
footing:
	swipl -s footing.pl -g run_tests -g run

.PHONY: forall
forall:
	swipl -s forall.pl 

.PHONY: gma
gma:
	swipl -s gma.pl -g main

.PHONY: gma_example
gma_example:
	swipl -s gma_example.pl gma.pl

.PHONY: goals
goals:
	swipl -s goals.pl -g print_sorted_goals -t halt

.PHONY: guitars
guitars:
	swipl -s guitars.pl

.PHONY: inheritance
inheritance:
	swipl -s inheritance.pl

.PHONY: inches_from_feet
inches_from_feet:
	swipl -s inches_from_feet.pl

.PHONY: joe
joe:
	./joe

.PHONY: json
json:
	swipl -s json.pl

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

.PHONY: live
live:
	swipl -s live.pl -g main -t halt

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
	swipl -s piano_setup.pl -g run -t halt

.PHONY: planning
planning:
	swipl -s planning.pl

.PHONY: phrase_string
phrase_string:
	swipl -s phrase_string.pl

.PHONY: phrase_to_file
phrase_to_file:
	swipl -s phrase_to_file.pl 

.PHONY: phrase_to_file_example
phrase_to_file_example:
	swipl -s phrase_to_file_example.pl -g main -g book -t halt

.PHONY: prologscript
prologscript:
	swipl -s prologscript.pl

.PHONY: pure_io
pure_io:
	swipl -s pure_io.pl

.PHONY: rando
rando:
	./rando.pl 
	./rando.pl 1000
	./rando.pl 250 300

.PHONY: reverse
reverse:
	swipl -s reverse.pl

.PHONY: scraping
scraping:
	scryer-prolog scraping.pl

.PHONY: script
script:
	./script.pl joe legner

.PHONY: second
second:
	swipl -s second.pl

.PHONY: structural
structural:
	swipl -s structural.pl

.PHONY: svg
svg:
	./svg.pl

.PHONY: swift
swift:
	./swift.pl > swift.swift
	swift swift.swift

.PHONY: system
system:
	swipl -s system.pl

.PHONY: testing_example
testing_example:
	swipl -s testing_example.pl

.PHONY: time
time:
	swipl -s time.pl

.PHONY: tree
tree:
	swipl -s tree.pl

.PHONY: twitch
twitch:
	swipl -s twitch.pl -g main -t halt

.PHONY: twt_setup
twt_setup:
	swipl -s twt_setup.pl

.PHONY: universe
universe:
	swipl -s universe.pl

.PHONY: units
units:
	swipl -s units.pl

.PHONY: zag
zag:
	swipl -s axiomatic_design -g print_zag_templates -t halt | pbcopy
	@echo 'Copied zig templates to clipboard with pbcopy'

.PHONY: zig
zig:
	swipl -s axiomatic_design -g print_zig_templates -t halt | pbcopy
	echo 'Copied zig templates to clipboard with pbcopy'

.PHONY: serial_number
serial_number:
	swipl -s serial_number.pl -g run

.PHONY: storage
storage:
	swipl -s storage.pl

.PHONY: shell_output
shell_output:
	swipl -s shell_output.pl

.PHONY: bratko
bratko:
	swipl -s bratko.pl

.PHONY: combination
combination:
	swipl -s combination.pl
