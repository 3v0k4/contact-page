#!/bin/bash

OUTPUT="timeless-software-wisdom.pdf"

pandoc doc.markdown \
  -B before.tex \
  -A after.tex \
  -o "${OUTPUT}" \
  -H headers.tex \
  --toc \
  --pdf-engine=xelatex \
  -V urlcolor:blue \
  -V fontsize=12pt \
  -V geometry:a4paper

# Latex removes the links from the intermission.pdf pages.
# We replace the current intermissions with the original ones with the working links.

# Array of page numbers
intermissions=( $(pdfgrep -n "SHOCKED BY THE QUALITY" "${OUTPUT}" | cut -d':' -f1) )
first="true"
number_of_pages=`qpdf --show-npages ${OUTPUT}`
command="qpdf --replace-input --pages ${OUTPUT} "

for ((i = 1; i <= $number_of_pages; ++i)); do
  if [[ " ${intermissions[*]} " =~ " ${i} " ]]; then
    command+=" intermission.pdf 1 ${OUTPUT} "
    first="true"
  else
    if [[ "${first}" == "true" ]]; then
      first="false"
      command+="${i}"
    else
      command+=",${i}"
    fi;
  fi
done

command+=" -- ${OUTPUT}"
eval "${command}"
