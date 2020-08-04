#!/bin/sh

## compile book:
R -e  'bookdown::render_book("index.Rmd", "bookdown::pdf_book")'
