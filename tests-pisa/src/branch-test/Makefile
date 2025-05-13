#
# Makefile for branch-test
#

# SimpleScalar directory
SS_DIR = ../../..

# Target binary names
PROG = branch-test

# Source files
SRCS = branch-test.c

# Default target
all: $(PROG)

# Compile for PISA target
$(PROG): $(SRCS)
	gcc -O0 -o $(PROG) $(SRCS)

# Run with SimpleScalar PISA simulator
run: $(PROG)
	../../../sim-pipe -max:inst 1000 ./$(PROG)

# Run with branch pipeline view
branch-view: $(PROG)
	../../../pipe-view.sh -max:inst 1000 ./$(PROG)

# Clean up
clean:
	rm -f $(PROG) *.o

.PHONY: all run branch-view clean 