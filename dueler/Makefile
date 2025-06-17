# Makefile for generating DuelerMVP Swift source code from Prolog DCG

.PHONY: all run clean

# Output file
OUTPUT = DuelerMVP.swift

# Main Prolog file
MAIN = duelermvp/duelermvp.pl

all: run

run:
	swipl -s duelermvp.pl -g run -t halt
	swipl -s app.pl -g run -t halt

# Optional: if you want to redirect to a Swift source file
write:
	swipl -q -t "use_module('$(MAIN)'), phrase(duelermvp:source_code, Program), open('$(OUTPUT)', write, S), maplist(writeln(S), Program), close(S), halt."

clean:
	rm -f $(OUTPUT)
