#!/bin/bash
#shell script for changing all file names in a directory
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

# Loop through files starting with "xyz" in the specified directory
for FILE in "$DIRECTORY"/hits_boundarywrap_*; 
do
    # Check if the file exists
    if [ -e "$FILE" ]; then
        # Extract the base name and directory name
        BASENAME=$(basename "$FILE")
        DIRNAME=$(dirname "$FILE")

        # Remove 'xyz' from the beginning and replace it with 'abc'
        NEWBASENAME="targetmoves_${BASENAME#hits_boundarywrap_}"

        # Handle file extension
        EXTENSION="${NEWBASENAME##*.}"
        FILENAME="${NEWBASENAME%.*}"

        # Check if the file has an extension
        if [ "$FILENAME" == "$NEWBASENAME" ]; then
            # No extension
            NEWNAME="$DIRNAME/${NEWBASENAME}_10"
        else
            # With extension
            NEWNAME="$DIRNAME/${FILENAME}_10.$EXTENSION"
        fi

        # Rename the file
        mv "$FILE" "$NEWNAME"

        # Print the old and new file names
        echo "Renamed: $FILE to $NEWNAME"
    fi
done


#find /usr/local/bin/ -name "*.sh"
#chmod +x rename_files.sh
#changed working directory to where the shell script and then 
#ran rename_files.sh with the directory to change 
#./rename_files.sh /path/to/directory
