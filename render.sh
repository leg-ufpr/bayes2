#!/bin/bash

set -e

if [ $# -eq 0 ]; then
  echo "Rendering all Rmd files..."
  Rscript -e "rmarkdown::render_site()"
elif [ $# -eq 1 ]; then
  FILE="$1"
  if [ ! -f "$FILE" ]; then
    echo "Error: File '$FILE' not found."
    exit 1
  fi
  echo "Rendering '$FILE'..."
  Rscript -e "rmarkdown::render('$FILE')"
  HTML="${FILE%.Rmd}.html"
  mkdir -p docs
  if true; then
    mv "$HTML" "docs/$HTML"
    echo "Moved '$HTML' to docs/."
  fi
else
  echo "Usage: $0 [file.Rmd]"
  exit 1
fi

echo "Done."
