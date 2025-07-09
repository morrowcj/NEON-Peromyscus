# This script renders the github pages (./docs/)

# render the site
bookdown::render_book("index.Rmd")

## Options for the output of the book are split between the YAML header of the
## index.Rmd file and the _bookdown.yml file. The latter is where the individual
## .Rmd "chapters" are specified, as well as the output location ("docs")

# remove the unneccessary files
unlink("neon-peromyscus_files", recursive = TRUE)
