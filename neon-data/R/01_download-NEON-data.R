# ---- parameters ----

products <- c(
  "mammal-trap-data" = "DP1.10072.001",
  "beetle-pitfall-data" = "DP1.10022.001",
  "plant-presence-and-cover" = "DP1.10058.001",
  "rodent-hanta-virus" = "DP1.10064.001",
  "rodent-tickborne-pathogen-status" = "DP1.10064.002",
  "tick-pathogen-status" = "DP1.10092.001",
  "tick-drag-cloth-sampling" = "DP1.10093.001",
  "weather-summaries" = "DP4.00001.001",
  "plant-phenology-observations" = "DP1.10055.001",
  "mammal-DNA-barcodes" = "DP1.10076.001",
  "litterfall-data" = "DP1.10033.001",
  "vegetative-structure" = "DP1.10098.001"
)
prod_names <- names(products)

# these ones require a different download process
large_products <- c(
  "relative-humidity" = "DP1.00098.001",
  "precipitation" = "DP1.00006.001",
  "biological-temperatures" = "DP1.00005.001",
  "air-temperatures" = "DP1.00003.001"
  # "meteorological-products" = "DP4.00200.001", # bundled eddy-covariance (very large)
  # "phenology-images" = "DP1.00033.001", # TODO need to download from different source?
)
large_names <- names(large_products)

start_date <- NA
end_date <- NA
pack <- "basic"
release <- "current"
sites <- c("HARV", "ORNL", "SCBI", "BLAN", "SERC", "STEI", "UNDE", "MLBS")
prov <- TRUE
check <- FALSE
# save_dir <- "R:/morrow5_data_repo/NEON-products/eight_sites_data" 
save_dir <- "neon-data/data/eight_sites/"
# api_token <- readLines("docs/NEON-API-token-morrow5.txt", n = 1, warn = FALSE)
ncores <- 2
check_size <- FALSE
force_down <- FALSE
rm_after <- TRUE
verbose  <- TRUE

# ---- setup ----

source("neon-data/R/00_NEON-API-token.R")
.setNeonTokenGlobal(file = "NEON-API-token-morrow5.txt")

# load packages
pkgs <- c("neonUtilities") # packages to load
pacman::p_load(pkgs, character.only = TRUE) # load with pacman

# create the save directory if it doesn't exist
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# ---- download data ----

for (i in seq_len(length(products))) {
  prod <- products[i]
  prod_name <- prod_names[i]

  # build file name
  file_name <- paste0(prod_name, ".rds")
  file_path <- file.path(save_dir, file_name)

  # skip this loop iteration if the download is not needed
  if (file.exists(file_path) & !force_down) {
    next
  }

  # tell user which product is downloading
  if (verbose) {
    message(paste0("Product: ", prod_name, " (", prod, ")"))
  }
  
  # load the product into memory, from the NEON data store
  data_object <- loadByProduct(
    dpID = prod, site = sites, startdate = start_date, enddate = end_date,
    package = pack, release = release, include.provisional = prov,
    check.size = check_size, nCores = ncores, token = NEON_TOKEN
  )

  saveRDS(data_object, file = file_path)
}

# ---- download the large products ----
for (i in seq_len(length(large_products))) {
  
  # product info
  prod <- large_products[i]
  prod_name = large_names[i]
  
  # setup the subfolder in the save directory
  large_dir <- file.path(save_dir, prod_name)
  
  # create the subfolder if missing
  if (!dir.exists(large_dir)) {
    dir.create(large_dir, recursive = TRUE)
  }
  
  # loop through each site 
  for (j in seq_len(length(sites))) {
    
    # site info
    site = sites[j]
    
    # break loop if "all" sites are requested
    if (grepl("all", site, ignore.case = TRUE)) {
      break
    }
    
    # build path for this site
    file_name <- paste0(prod_name, "_", site, ".rds")
    
    # store in the subfolder
    file_path <- file.path(large_dir, file_name)
    
    # skip this loop iteration if the download is not needed
    if (file.exists(file_path) & !force_down) {
      next
    }
    
    # tell the user which product is downloading
    if (verbose) {
      message(paste0("Product: ", prod_name, " at site ", site," (", prod, ")"))
    }
    
    # load the product
    data_object <- loadByProduct(
      dpID = prod, site = site, startdate = start_date, enddate = end_date,
      package = pack, release = release, include.provisional = prov,
      check.size = check_size, nCores = ncores, token = NEON_TOKEN
    )
    
    # save the product
    saveRDS(data_object, file = file_path)
  }
}

# # ---- if sourced as a script ----
#
# if (sys.nframe == 0) { # only run if executed from console
#   rm(data_object)
# }
