#!/bin/sh

R -e  'bookdown::render_book("index.Rmd", "bookdown::gitbook")'
