EXEC = determine
PROLOG_SRC = determine.pl
PDF_FILE = report.pdf

.PHONY: executable clean

$(EXEC): $(PROLOG_SRC)
	swipl --goal=main --stand_alone=true -o $(EXEC) -c $(PROLOG_SRC)

run: $(EXEC)
	./$(EXEC) report.json

clean:
	rm -f $(EXEC)

.PHONY: run

open: run
	open $(PDF_FILE)