.PHONY: all clean run tests

all: clean run

test: part1 tests
	@for t in `cat tests`; do echo $$t | ./part2 ; done

run: part2
	@./part2

%: %.hs
	@ghc $^ > /dev/null

clean:
	rm -f part{1,2}{,.hi,.o}
