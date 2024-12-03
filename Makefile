# Variables
CABAL_CMD = cabal run ebpf-cfg --
EXAMPLES = examples/add.asm \
           examples/sum_five.asm \
           examples/categorize_and_sum.asm \
           examples/count_bits.asm \
           examples/factorial5.asm \
           examples/gcd.asm \
           examples/is_prime.asm \
           examples/max_value.asm \
           examples/sum_digits.asm \
           examples/sum_even10.asm

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

# Clean up build artifacts
clean:
	cabal clean
