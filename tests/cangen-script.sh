#!/bin/sh

path_to_datafile=./test-data

while IFS= read -r line; do
  $line
done < $path_to_datafile

