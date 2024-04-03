#!/bin/bash

##################################################################
# The bash script is called by forTof18.exe to gather the list
# of the file locate in your command directories.
##################################################################

fpath="$(pwd)"  # Replace this withthe actual folder path
fini=${#fpath}+1
output_file="for_file_list.txt"
rm $output_file
farray=()

# Loop through all files in the folder
for file in "$fpath"/*.for; do
    # Check if the file exists and has the ".for" extension
    if [ -f "$file" ]; then
        farray+=("$file")
    fi
done

# Print the filenames in the array
alen=${#farray}
echo "-------------------"
echo "Number of file: $(alen)"
echo "-------------------"
echo " "
echo "$alen" >> "$output_file"
for file_name in "${farray[@]}"; do
    fend=${#file_name}
    echo "${file_name:$fini:$fend}"
    echo "${file_name:$fini:$fend}" >> "$output_file" 
done

