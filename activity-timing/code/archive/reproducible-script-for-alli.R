# file path
data_dir <- "data/raw-iButton-data/"
file_name <- "STEI_05_27_100s_downloaded_06_12.csv"
file_path = file.path(data_dir, file_name)

# read the data
dat <- read.csv(file = file_path, check.names = FALSE)
dim(dat)

# check for duplicated columns
dup_cols <- which(duplicated(t(dat)))

# print the duplicated column indices
unname(dup_cols)

# and look at the duplicated column names
colnames(dat[dup_cols]) # there appear to be some excel scientific notation issues

# remove the duplicated columns
deduped <- dat[, -dup_cols]

# check dimensions
dim(deduped) # very few columns...

# check that the dimensions are correct
ncol(dat) - ncol(deduped) == length(dup_cols)

