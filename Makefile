.PHONY: all
all: go chords practice kowalski world physics

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
