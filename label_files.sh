#!/bin/bash
#
# ChatGPT wrote this at Joe Legner's prompting on May 13, 2025.
# This version does not change files — it only reports what would happen.

total=0
to_update=0
skipped=0

for file in *.pl; do
    total=$((total + 1))
    first_line=$(head -n 1 "$file")
    filename_comment="% $file"

    if [ "$first_line" != "$filename_comment" ]; then
        echo "Would update $file"
        to_update=$((to_update + 1))
    else
        echo "Would skip $file (already has filename comment)"
        skipped=$((skipped + 1))
    fi
done

echo
echo "Summary:"
echo "  Out of $total .pl files:"
echo "  Modified: $to_update"
echo "  Skipped:  $skipped"
