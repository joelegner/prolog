#!/bin/zsh

# Check for required argument
if [[ -z "$1" ]]; then
  echo "Usage: $0 <filename (without extension)>"
  exit 1
fi

FILENAME="$1"
PLFILE="${FILENAME}.pl"
MAKEFILE="Makefile"

# Create the .pl file if it doesn"t already exist
if [[ -e "$PLFILE" ]]; then
  echo "File $PLFILE already exists."
else
  touch "$PLFILE"
  echo "Created $PLFILE"
fi

# Append to Makefile
echo "" >> "$MAKEFILE"
echo ".PHONY: $FILENAME" >> "$MAKEFILE"
echo "$FILENAME:" >> "$MAKEFILE"
echo "\tswipl -s $PLFILE" >> "$MAKEFILE"

echo "Added $FILENAME target to $MAKEFILE"
