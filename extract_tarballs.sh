#!/bin/bash

# Set the directory containing the tarballs
current_directory="/projects/my-private-bucket/run_output_20241115_131005/"

# Change to the specified directory
cd "$current_directory" || { echo "Directory not found!"; exit 1; }

# Iterate through all tar.gz files in the directory
for file in *.tar.gz; do
    # Skip if no .tar.gz files are found
    if [ ! -e "$file" ]; then
        echo "No .tar.gz files found!"
        exit 1
    fi

    # Extract the base name (e.g., TEAK_large_021)
    base_name=$(echo "$file" | sed -E 's/_point_clouds\.tar\.gz$//')

    # Create a new directory
    mkdir -p "$base_name"

    # Move the tarball into the new directory
    mv "$file" "$base_name/"

    # Extract the tarball using pigz
    tar --use-compress-program=pigz -xvf "$base_name/$file" -C "$base_name/"

    echo "Extracted $file into $base_name/"
done
