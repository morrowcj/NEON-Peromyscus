# This script renders the github pages (./docs/)

# render the site
## the "params" get passed through to all RMD files
bookdown::render_book(
  input = "index.Rmd", config_file = "_bookdown.yml",
  params = list(
    run_scripts = FALSE,
    force = TRUE,
    cache_dir = "infection-modeling/R/rmarkdown/cache/resistance-tolerance"
  )
)

# lines with TBD/TBA or TODO
mdowns = suppressWarnings(
  system('grep -E "(TBD|TBA|TODO)" infection-modeling/R/rmarkdown/*.Rmd', TRUE)
)
scripts = suppressWarnings(
  system('grep -E "(TBD|TBA|TODO)" infection-modeling/R/scripts/*.R', TRUE)
)
if (length(mdowns > 0)) {
  warning("TODO's present in Rmarkdown files.")
  print(mdowns)
}
if (length(scripts > 0)) {
  warning("TODO's present in R scripts files.")
  print(scripts)
}
## Options for the output of the book are split between the YAML header of the
## index.Rmd file and the _bookdown.yml file. The latter is where the individual
## .Rmd "chapters" are specified, as well as the output location ("docs")

# remove the unnecessary files
# unlink("neon-peromyscus_files", recursive = TRUE)
