#!/bin/bash
# branch-view.sh - Run sim-pipe and highlight branch behavior
# Usage: ./branch-view.sh [options] [sim-pipe options] program [args]

# Default settings
MAX_INST=${MAX_INST:-100}
BRANCH_ONLY=false
HELP=false

# Process our script options first
while (( "$#" )); do
  case "$1" in
    -h|--help)
      HELP=true
      shift
      ;;
    -b|--branch-only)
      BRANCH_ONLY=true
      shift
      ;;
    --) # end argument parsing
      shift
      break
      ;;
    *) # preserve remaining arguments
      break
      ;;
  esac
done

# Display help
if $HELP; then
  echo "Usage: ./branch-view.sh [options] [sim-pipe options] program [args]"
  echo ""
  echo "Options:"
  echo "  -h, --help        Show this help message"
  echo "  -b, --branch-only Only show pipeline activity around branches"
  echo ""
  echo "Environment variables:"
  echo "  MAX_INST=<num>    Set number of instructions to simulate (default: 100)"
  echo ""
  echo "Examples:"
  echo "  ./branch-view.sh tests-pisa/bin.little/test-math"
  echo "  ./branch-view.sh -b tests-pisa/bin.little/test-math"
  echo "  MAX_INST=200 ./branch-view.sh tests-pisa/bin.little/test-math"
  exit 0
fi

# Run the simulator and capture output
output=$(./sim-pipe -quiet -max:inst $MAX_INST "$@" 2>/dev/null | grep -E '^(fetch|decod|exec |mem  |wb   ):')

# Replace invalid instructions with PIPELINE STALL message
processed_output=$(echo "$output" | sed 's/<invalid inst: 0x00000000:00000000>/*** PIPELINE STALL - WAITING FOR BRANCH ***/g')

# Display header
echo "=== Branch Pipeline Visualization (No Branch Prediction - Stall Model) ==="
echo "Stage   Cycle #  Inst #    Address         Assembly Code"

# Process and display the output
if $BRANCH_ONLY; then
  # If branch-only mode, extract and display only branches and surrounding instructions
  echo "$processed_output" | grep -E -A 5 -B 5 '(jal|jr|j |beq|bne|blez|bgez|bgtz|bltz)' | 
    grep -E '^(fetch|decod|exec |mem  |wb   ):' |
    # Highlight branch instructions and pipeline stalls
    grep --color=always -E '(jal|jr|j |beq|bne|blez|bgez|bgtz|bltz|\*\*\* PIPELINE STALL)|(^.*$)'
else
  # Regular mode, show all instructions
  echo "$processed_output" | 
    # Highlight branch instructions and pipeline stalls
    grep --color=always -E '(jal|jr|j |beq|bne|blez|bgez|bgtz|bltz|\*\*\* PIPELINE STALL)|(^.*$)'
fi

# Show branch statistics
echo -e "\n=== Branch Analysis Summary ==="

branch_count=$(echo "$output" | 
  grep -E '^(exec |mem  |wb   ):' | 
  grep -E '(jal|jr|j |beq|bne|blez|bgez|bgtz|bltz)' | 
  grep -v "<invalid" | wc -l)

stall_count=$(echo "$output" | 
  grep -E '^(fetch|decod|exec |mem  |wb   ):' | 
  grep "<invalid" | wc -l)

echo "Branch instructions found: $branch_count"
echo "Pipeline stalls: $stall_count"

# Calculate ratio only if branch_count is greater than 0
if [ "$branch_count" -gt 0 ]; then
  # Simple integer division without using bc
  ratio=$((stall_count / branch_count))
  echo "Stall/branch ratio: $ratio stall cycles per branch"
else
  echo "Stall/branch ratio: N/A (no branches executed)"
fi 