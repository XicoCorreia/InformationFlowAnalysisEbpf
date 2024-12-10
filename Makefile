CABAL_CMD = cabal run ebpf-cfg --
EXAMPLES = examples/sumFive.asm \
           examples/doWhile.asm \
           examples/whileLoop.asm \
           examples/ifStatement.asm \
		   examples/nestedIfLoop.asm \

EXAMPLE_NAME = sumFive

# Default target
all: run-tests

# Build using cabal
build:
	cabal build

# Run the program for each example
run-tests: build
	@for file in $(EXAMPLES); do \
		echo "Running $$file"; \
		$(CABAL_CMD) $$file; \
	done

run-test:
	$(CABAL_CMD) examples/$(EXAMPLE_NAME).asm

generate_pdf: 
	$(CABAL_CMD) examples/$(EXAMPLE_NAME).asm graphs/$(EXAMPLE_NAME).dot
	dot -Tpdf graphs/$(EXAMPLE_NAME).dot -o graphs/$(EXAMPLE_NAME).pdf

# Clean up build artifacts
clean:
	cabal clean
