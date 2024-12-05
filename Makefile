CABAL_CMD = cabal run ebpf-cfg --
EXAMPLES = examples/add.asm \
           examples/sum_five.asm \
           examples/count_bits.asm \
           examples/factorial5.asm \
           examples/gcd.asm \
           examples/max_value.asm \
           examples/sum_digits.asm \
           examples/sum_even10.asm

EXAMPLE_NAME = add

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
