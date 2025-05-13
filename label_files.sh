#!/bin/bash
#
# ChatGPT wrote this at my prompting on May 13, 2025.
# Here is the prompt:
#
#I have a bunch of Prolog files with .pl suffix. I want the top of each file to be the
# filename in a comment. % accounting.pl
#
# Exmaple usage from May 13, 2025:
#
# % ./label_files.sh 
# Updating accounting.pl
# Updating act.pl
# Updating axiology.pl
# Skipping axiomatic_design.pl (already has filename comment)
# Skipping basics.pl (already has filename comment)
# ...
#
# Cycle through .pl files and add first comment:
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
