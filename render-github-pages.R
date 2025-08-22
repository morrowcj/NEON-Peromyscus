# This script renders the github pages (./docs/)

# render the site
## the "params" get passed through to all RMD files
bookdown::render_book(
  input = "index.Rmd", config_file = "_bookdown.yml", 
  new_session = FALSE,
  params = list(
    run_scripts = FALSE,
    force = TRUE, 
    cache_dir = "infection-modeling/R/rmarkdown/cache/resistance-tolerance"
  ), 
)

## Options for the output of the book are split between the YAML header of the
## index.Rmd file and the _bookdown.yml file. The latter is where the individual
## .Rmd "chapters" are specified, as well as the output location ("docs")

# remove the unneccessary files
# unlink("neon-peromyscus_files", recursive = TRUE)
