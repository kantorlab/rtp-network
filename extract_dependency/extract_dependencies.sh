#!/bin/bash

for file in ../*.R ../*.Rmd; do
  echo "File: $file"
  
  # Extract package dependencies
  echo "Package Dependencies:"
  grep -E 'library\(|require\(' "$file" | sed -r "s/.*\((.*)\).*/\1/" | sed "s/['\"]//g"
  
  # Extract file dependencies
  echo "File Dependencies:"
  grep -E 'read.csv|load|source|readRDS|read.table|read.delim|read_tsv|read_csv' "$file" | sed -r "s/.*\((.*)\).*/\1/" | sed "s/['\"]//g"
  
  echo "------"
done




