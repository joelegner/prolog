.PHONY: go
go: 
	swipl -s htmlgen.pl -g go -t halt

.PHONY: chords
chords:
	swipl -s chords.pl -g main

.PHONY: world
world:
	swipl -s world.pl -g main -t halt
