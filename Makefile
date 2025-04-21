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
