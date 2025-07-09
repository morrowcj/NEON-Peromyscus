# This script renders the github pages (./docs/)

# ---- setup ----

# collect the paths of markdown files to convert into the webpage
render_paths <- c(
  "infection-modeling/R/rmarkdown/resistence-tolerance.Rmd"
)

# absolute paths to key directories
root_dir = R.utils::getAbsolutePath(".")
page_dir = R.utils::getAbsolutePath("docs")

## ---- Render Rmd files ----

# initialize progress bar
pb = txtProgressBar(style = 3)

# loop through each rmd file...
for (i in seq_len(length(render_paths))) {
  
  # absolute path to this RMD file
  file_path = R.utils::getAbsolutePath(render_paths[i])
  
  # render this rmd to the page_dir, with .md files
  rmarkdown::render(
    input = file_path,
    output_format = bookdown::html_document2(keep_md = TRUE), 
    knit_root_dir = root_dir,
    output_dir = page_dir
  )
  
  # update progress bar
  setTxtProgressBar(pb, i)
}

## ---- build index / web structure ----

rmarkdown::render_site(
  input = page_dir
)
