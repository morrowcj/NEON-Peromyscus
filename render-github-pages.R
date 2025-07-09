# This script renders the github pages (./docs/)

# collect the paths of markdown files to convert into the webpage

render_paths <- c(
  "infection-modeling/R/rmarkdown/resistence-tolerance.Rmd"
)

root_dir = R.utils::getAbsolutePath(".")

pb = txtProgressBar(style = 3)
for (i in seq_len(length(render_paths))) {
  
  file_path = R.utils::getAbsolutePath(render_paths[i])
  
  rmarkdown::render(
    input = file_path,
    output_format = bookdown::html_document2(keep_md = TRUE), 
    knit_root_dir = root_dir,
    output_dir = "docs"
  )
  
  setTxtProgressBar(pb, i)
}
