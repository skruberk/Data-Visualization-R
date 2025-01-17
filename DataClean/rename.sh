#!/bin/bash
# Shell script to rename files from bird_* to cat_*

# Check if the correct number of arguments is provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <directory>"
    exit 1
fi

# Capture the directory
DIRECTORY=$1

# Check if the provided argument is a valid directory
if [ ! -d "$DIRECTORY" ]; then
    echo "Error: $DIRECTORY is not a valid directory."
    exit 1
fi

# Enable nullglob to avoid literal patterns if no matches
#put pattern to change from here
shopt -s nullglob
FILES=("$DIRECTORY"/eGFP_*)

# Check if there are any files to rename
if [ "${#FILES[@]}" -eq 0 ]; then
    echo "No files matching the pattern' found in $DIRECTORY"
    exit 0
fi

# Loop through the files and rename them
for FILE in "${FILES[@]}"; do
    # Extract the directory and base name of the file
    DIRNAME=$(dirname "$FILE")
    BASENAME=$(basename "$FILE")

    # Remove cat_* and replace it with 'bird_'
    NEWBASENAME="brolga${BASENAME#eGFP}"

    # Construct the new file name
    NEWNAME="$DIRNAME/$NEWBASENAME"

    # Rename the file
    if mv "$FILE" "$NEWNAME"; then
        echo "Renamed: $FILE to $NEWNAME"
    else
        echo "Failed to rename: $FILE"
    fi
done


# cd /cygdrive/o/Raw_Data/Kristen
#use cygwin for this
#./rename.sh BACT_t1     call this from cygwin to rename 
#cd /cygdrive/c

