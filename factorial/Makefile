EXEC = factorial_exec
PROLOG_SRC = factorial.pl main.pl

.PHONY: executable clean

$(EXEC): $(PROLOG_SRC)
	swipl --goal=main --stand_alone=true -o $(EXEC) -c $(PROLOG_SRC)

run: $(EXEC)
	./$(EXEC)

clean:
	rm -f $(EXEC)
