# NEON Peormyscus Project (Structural equation modeling of zoonotic disease risk)

This repository houses the data, code, and related documents associated with
our project wherein we investigate the impacts of individual *Peromyscus* and
environmental variation on Lyme disease (*Borrelia burgdorferi*) risk.

To download this project locally, click the green "<> Code" dropdown and select
"Download ZIP".

## Project structure

Some important files that are *not* related to the analysis are `index.Rmd`,
`render-github-pages.R`, and `_bookdown.yml`, which are responsible for 
generating the files in the `docs/` folder, which form the 
[Project GitPages website](https://morrowcj.github.io/NEON-Peromyscus).

The main project structure is contained in three main sub-folders, 
documented in the following subsections:

### neon-data

This folder contains data collected from NEON, and the code to acquire and 
pre-process it. Data is housed in `data/` and code in `R/`. The R scripts were
executed in sequential order (00 - 03) and depend upon the `download-function.R`
script. Data are saved as R objects `.rds` that can be loaded into R with the
`readRDS` function.

The `data/eight_sites/` folder contains data at the eight NEON sites used
by the project. The sub-folders contain data that were downloaded on a per-site
basis (e.g., `precipitation/precipitation_BLAN.rds`). The bare files are include
data for all sites. Some of these files are created as the result of pre-processing
the raw data.

### activity-timing

This folder contains data pertaining to the ibutton-derived capture timing
estimates and code used to process and prepare those data. Data is housed in
`data/`, code is housed in `R/` and grouped into `functions/` 
(R functions created for this project) and `scripts`, which contains the file
that was used to estimate capture times and depends upon these functions.

The `data/ibuttons/` folder contains raw data 
(`raw-temperatures/`, `raw-metadata/`), cleaned data 
(`cleaned-metadata/`, `cleaned-temepratures/`), and output from the processing
script. 

The `capture-time-estimates_2022-2024.rds` file contains the combined capture
time estimates from all years of the study.

### infection-modeling

This is the primary folder of this project involved in data analysis and 
manuscript preparation. It is grouped into 5 sub-folders: 

`R`: This contains all the code for the project, grouped by `functions` 
(as above, these are R functions that fascilitate the analyses), `scripts` 
(the code used to perform the analyses), and `rmarkdown` (the documents used
to present information, and create the GitPages website). In general, the 
files with numeric prefixes were executed sequentially (e.g., 01-08) and the
other files are standalone (but sometimes depend on output from other files).

`data`: this is where the data used for this project are stored. The files are
often output from the R code, but also contain raw data seperate from the raw
NEON and activity timing data.

`graphical-models` contains some preliminary SEM diagrams and `graphics`
contains figures generated for the manuscript (i.e., via 
`build-MS-figs-and-tables.R`).

`manuscript`: this is where some of the files associated with manuscript
preparation are stored. But the primary manuscript file is housed 
on [OneDrive](https://uwprod-my.sharepoint.com/:w:/g/personal/morrow5_wisc_edu/IQDQTRZxsv56S6axYGH--8sTAYGBE-UFmkFNkwYmaQTWVzk).

## Website

To see summaries of our analyses, see [our GitPages website](https://morrowcj.github.io/NEON-Peromyscus/).
