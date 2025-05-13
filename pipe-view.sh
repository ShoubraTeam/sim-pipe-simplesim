#!/bin/bash
# pipe-view.sh - Run sim-pipe and filter output to show only pipeline stages
# Usage: ./pipe-view.sh [sim-pipe options]

# Print the header
echo "Stage   Cycle #  Inst #    Address         Assembly Code"

# Run sim-pipe with all options passed to this script
# Use a proper regex to match all stages
./sim-pipe -quiet "$@" 2>/dev/null | grep -E '(fetch|decod|exec |mem  |wb   ):' 