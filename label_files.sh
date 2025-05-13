#!/bin/bash

for file in *.pl; do
    first_line=$(head -n 1 "$file")
    filename_comment="% $file"

    if [ "$first_line" != "$filename_comment" ]; then
        echo "Updating $file"
        tmpfile=$(mktemp)
        {
            echo "$filename_comment"
            cat "$file"
        } > "$tmpfile"
        mv "$tmpfile" "$file"
    else
        echo "Skipping $file (already has filename comment)"
    fi
done
