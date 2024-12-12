CABAL_CMD = cabal run ebpf-cfg --
EXAMPLES = doWhile \
           whileLoop \
           ifStatement \
		   nestedIfLoop \
		   nestedWhiles \
		   seqWhiles \
		   doWhileIfNested \

EXAMPLE_NAME = doWhile

# Default target
all: run-tests

# Build using cabal
build:
	cabal build

# Run the program for each example
run-tests: build
	@for file in $(EXAMPLES); do \
		$(CABAL_CMD) examples/$$file.asm graphs/$$file.dot; \
		dot -Tpdf graphs/$$file.dot -o graphs/$$file.pdf; \
	done

run-test:
	$(CABAL_CMD) examples/$(EXAMPLE_NAME).asm graphs/$(EXAMPLE_NAME).dot
	dot -Tpdf graphs/$(EXAMPLE_NAME).dot -o graphs/$(EXAMPLE_NAME).pdf

# Clean up build artifacts
clean:
	cabal clean
	rm -r graphs/*
