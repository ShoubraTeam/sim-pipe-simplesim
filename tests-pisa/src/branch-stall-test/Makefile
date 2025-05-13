#
# Makefile for branch-stall-test benchmark
#

# Source files
PROG = branch-stall-test
SRCS = $(PROG).c

# Options
CC = gcc
CFLAGS = -O0 -g

# Target directories
BIN_DIR = ../../bin.little
SRC_DIR = .

# Default target
all: $(PROG)

# Compile the program
$(PROG): $(SRCS)
	$(CC) $(CFLAGS) -o $(PROG) $(SRCS)

# Install to SimpleScalar bin directory
install: $(PROG)
	cp $(PROG) $(BIN_DIR)/

# Create a binary through SimpleScalar simulation
simulate: $(PROG)
	@echo "Running $(PROG) through SimpleScalar simulator..."
	../../../sim-pipe -max:inst 1000 ./$(PROG)

# Run with branch stall visualization
branch-stall-view: $(PROG)
	@echo "Running $(PROG) with branch stall visualization..."
	../../../branch-view.sh -b -max:inst 1000 ./$(PROG)

# Clean up
clean:
	rm -f $(PROG) *.o

.PHONY: all install clean simulate branch-stall-view 