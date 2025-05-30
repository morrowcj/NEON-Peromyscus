# load packages for this
pacman::p_load(neonUtilities, neonOS, dplyr, lubridate)

ibutton_meta <- read.csv(
  "data/2024 - UW_Madison_Fob_Serial_Numbers.csv",
  colClasses = "character" # to avoid rounding digits
) |>
  # rename the columns
  setNames(c(
    "pair", "IO", "name", "new_pair", "SerialNumber", "pairStatus",
    "OK", "Notes"
  )) |>
  # convert pair and SN to factors
  mutate(pair = as.factor(pair), SerialNumber = as.factor(SerialNumber))

# get info about where ibuttons were deployed
ibutton_deployment <- read.csv("data/ibuttons_2024L0.csv") |>
  # pad pair labels with zeros on the left side, to match the metadata
  mutate(
    iButtonLabel = stringr::str_pad(
      string = iButtonLabel, width = 3, side = "left", pad = "0"
    )
  )

# raw ibutton data files
raw_folder <- "data/raw-iButton-data"
file_pattern <- "([A-Z]*)_(.*)_down*loaded_(.*)\\.csv"
all_files <- list.files(raw_folder, pattern = file_pattern)
nfiles <- length(all_files)

# quality control flag data frame
QC_frame <- data.frame(
  file = all_files, duplicates = NA, unmatched_SNs = NA, unpaired = NA
)
all_unmatched_SNs <- {} # SNs that aren't in ibutton_meta

# loop through the individual files
for (i in nfiles) {
  # get file info
  file <- all_files[i] # file name
  path <- file.path(raw_folder, file) # file path

  # load the file
  idat <- read.csv(path, check.names = FALSE) |>
    rename(DateTime = "Time")

  # handle duplicates
  QC_frame$duplicates[i] <- any(duplicated(idat))
  idat <- distinct(idat) # remove any duplicates

  # Get the date times for each row
  DateTimes <- lubridate::as_datetime(idat$DateTime)

  # remove the date time column (for now)
  idat <- idat |> select(-DateTime)

  # Get serial numbers from column names, and correct them
  SNs <- colnames(idat) # list of SNs
  SNs <- gsub("\\*", "", SNs) # remove "*"

  # check for unmatched Serial Numbers
  unmatched <- !SNs %in% ibutton_meta$SerialNumber # unmatched SNs
  if (any(unmatched)) {
    QC_frame$unmatched_SNs <- paste(SNs[unmatched], collapse)
    all_unmatched_SNs <- unique(c(all_unmatched_SNs, SNs[unmatched]))
  }

  # get the "pair" ID that corresponds to each SN
  pair_IDs <- ibutton_meta$pair[match(SNs, ibutton_meta$SerialNumber)]

  # check for unpaired buttons (or unspecified pairs)
  unpaired <- data.frame(pair = pair_IDs, SN = SNs) |>
    group_by(pair) |>
    tally() |>
    filter(n < 2)
  QC_frame$unpaired <- nrow(unpaired) > 0

  # transpose the data so that each row corresponds to a SN
  datmat <- unname(t(idat)) # also remove row and col names

  # calculate the differences between pairs

  main_threshold <- 1 # threshold for differences at a given time point
  run_len <- 10 # how many consecutive time points the threshold should be met
  max_threshold <- 3 # threshold for maximum temperature differences

  # build a data frame with a row for each pair
  pair_df <- data.frame(
    pair = unique(pair_IDs), SN1 = NA, SN2 = NA,
    max_met = NA, flagged_runs = NA,
    run_starts = NA, run_lengths = NA
  )

  for (p in seq_len(nrow(pair_df))) {
    # diff_met <- sapply(seq_len(nrow(pair_df)), function(p) {

    # get the pair info
    pair <- pair_df$pair[p]
    pair_index <- which(pair_IDs == pair)

    # skip this pair if there aren't exactly 2 buttons
    if (length(pair_index) != 2) {
      next
    }

    # get the SNs, and add them to the pair_df
    pair_df[p, c("SN1", "SN2")] <- pair_SNs <- SNs[pair_index]
    # get the pair subset
    matsub <- datmat[pair_index, ]

    # calculate differences
    diffs <- apply(matsub, 2, diff)
    abs_diffs <- abs(diffs) # absolute diff between date pairs
    max_diff <- max(abs_diffs, na.rm = TRUE) # max diff between date pairs

    # determine if the max threshold was met
    pair_df$max_met[p] <- max_diff >= max_threshold

    # determine if the main threshold was met, for each time point
    main_met <- abs_diffs >= main_threshold # is the threshold met each time?

    # look for all the consecutive runs (both TRUE and FALSE) in main_met
    equal_runs <- rle(main_met)

    # get the first run where TRUE (thresh met) happened the desired amount
    matching_stretches <- which(
      equal_runs$lengths >= run_len & equal_runs$values
    )

    # how many times were consecutive thresholds met?
    n_matches <- length(matching_stretches)

    run_starts <- {}
    run_lengths <- {}

    # loop through any matching stretches
    for (j in seq_len(n_matches)) {
      # get the first time our consecutive thresholds were met
      crit <- matching_stretches[j]
      # calculate the time period on which this occurs
      crit_start <- 1 + sum(equal_runs$lengths[1:(crit - 1)])
      # add this to the list of starts
      run_starts <- c(run_starts, crit_start)
      # get the run length
      run_lengths <- c(run_lengths, equal_runs$lengths[crit])
      # get the timepoints of the run
      run_times <- seq(crit_start, length.out = equal_runs$lengths[crit])
      # ensure that the stretch at this time point actually fits
      stopifnot(main_met[run_times])
    }

    pair_df$flagged_runs[p] <- n_matches
    pair_df$run_starts[p] <- paste(run_starts, collapse = ", ")
    pair_df$run_lengths[p] <- paste(run_lengths, collapse = ", ")

    # add the Serial Numbers and pair as a column
    df <- datmat |>
      data.frame() %>%
      setNames(object = ., seq_len(ncol(datmat))) |>
      # setNames(object = ., paste0("t", seq_len(ncol(datmat))))|>
      mutate(SN = SNs, pair = pair_IDs) |>
      relocate(c("pair", "SN"), .before = 0) |>
      arrange(pair)

    # save the data

    joined <- right_join(pair_df, df, by = c("pair"))

    # From here, start on line 572 from the iButton captimes siteID file

    dat <- readxl::read_excel("data/out_files/BLAN_23_1.xlsx", sheet = 1) |>
      filter(!is.na(iButtonLabel)) |>
      mutate(
        iButtonLabel = stringr::str_pad(
          iButtonLabel,
          width = 3, side = "left", pad = "0"
        ) |> as.factor()
      )

    # get the deployment index for each pair
    dep_inx <- which(ibutton_deployment$iButtonLabel %in% pair_df$pair)



    # # ## visualize
    # starts <- stringr::str_split(pair_df$run_starts[p], pattern = ", ") |>
    #   unlist() |>
    #   as.numeric()
    #
    # lens <- stringr::str_split(pair_df$run_lengths[p], pattern = ", ") |>
    #   unlist() |>
    #   as.numeric()
    #
    # ends = starts + lens - 1
    #
    # box_df <- data.frame(starts = DateTimes[starts],
    #                      ends = DateTimes[ends],
    #                      date = ymd(as.Date(DateTimes[starts]))) |>
    #   filter(date %in% ymd(as.Date(c("2024-04-02", "2024-04-03"))))
    #
    # df |>
    #   filter(pair == pair_df$pair[1]) |>
    #   tidyr::pivot_longer(c(-pair, -SN), names_to = "timepoint") |>
    #   mutate(
    #     datetime = DateTimes[as.numeric(timepoint)],
    #     date = ymd(as.Date(datetime))
    #   ) |>
    #   filter(date %in% ymd(as.Date(c("2024-04-02", "2024-04-03")))) |>
    #   ggplot(aes(x = datetime, y = value, group = SN)) +
    #   geom_vline(xintercept = DateTimes[starts],
    #              linetype = "dotted") +
    #   geom_rect(data = box_df, inherit.aes = FALSE,
    #             aes(xmin = starts, xmax = ends,
    #                 ymin = -Inf, ymax = Inf), fill = "grey90") +
    #   geom_line(aes(col = SN), size = 0.75) +
    #   theme_classic() +
    #   theme(legend.position = "none") +
    #   facet_wrap(~date, scales = "free_x", ncol = 1)
  } # )

  # # join with the ibutton data by serial number
  # joined_df <- left_join(df, ibutton_meta, by = c("SN" = "SerialNumber")) |>
  #   relocate(any_of(names(ibutton_meta)), .before = 2)
}







# ---- OLDER ----
ibutton_lookup <- read.csv("data/ibuttons_2024L0.csv")

mam_dat <- readRDS("../LymesPrediction/data/neon-data/mammal-trap-data.rds")
names(mam_dat)

trap_dat <- mam_dat$mam_pertrapnight |>
  mutate(link = paste0(uid, trapCoordinate)) |>
  filter(link %in% ibutton_lookup$link)

# ibutton deployment table

ibutton_lookup$iButtonLabel %in% ibutton_meta$pair

#
ibutton_data_files <- list.files("data/raw-iButton-data/")

missing_SNs <- {}

# for (i in 1) {
for (i in seq_len(length(ibutton_data_files))) {
  # read in the file
  ifile <- ibutton_data_files[i]
  ipath <- file.path("data/raw-iButton-data/", ifile)
  idat <- read.csv(ipath, header = TRUE, check.names = FALSE) |>
    rename(DateTime = "Time")

  # get a list of the dates, corresponding to the row ID
  date_time <- idat |>
    mutate(
      DateTime = ymd_hms(as_datetime(DateTime)),
      # Date = ymd(format(DateTime, "%Y-%m-%d")),
      # Time = format(DateTime, "%H:%M:%S"),
      .keep = "none"
    )

  # rename the indices
  ## remove the trailing . (asterix in original file) from column names
  colnames(idat) <- gsub("\\*$", "", colnames(idat))
  # ## remove the leading "X" caused by starting a column name with a digit
  # colnames(idat) <- gsub("^X", "", colnames(idat))
  # ## set the row names to the date times
  # rownames(idat) <- date_time$DateTime

  # list the serial numbers of the ibuttons
  SNs <- colnames(idat |> select(-DateTime))

  # warn if there are unmatched serial numbers
  if (any(!SNs %in% ibutton_meta$SerialNumber)) {
    misses <- SNs[!SNs %in% ibutton_meta$SerialNumber]
    missing_SNs <- unique(c(missing_SNs, misses))
    warning(paste(
      "Serial numbers", paste(misses, collapse = ", "),
      "do not match reference.", "(iteration", i, ")"
    ))
  }
}



# get the date range of the ibuttons
date_range <- gsub(
  pattern = "(\\d{4}-\\d{2})-\\d{2}", replacement = "\\1",
  x = range(as.Date(ibutton_lookup$endDate))
)



# sites (if known)
sites <- c("HARV", "ORNL", "SCBI", "BLAN", "SERC", "STEI", "UNDE", "MLBS")

# product ID of the small mammal box trap data
product_ID <- "DP1.10072.001"

# load the product into memory, from the NEON data store
product <- loadByProduct(
  startdate = date_range[1], enddate = date_range[2], dpID = product_ID,
  site = sites, package = "basic", include.provisional = TRUE,
  check.size = FALSE
)

# remove duplicates
product$mam_trapNight_nodups <- removeDups(
  product$mam_pertrapnight,
  variables = product$variables_10072,
  table = "mam_pertrapnight"
) |>
  suppressMessages()

saveRDS(product, "test-product.rds")
pp <- readRDS("test-product.rds")


# merge the ibutton data into (all) the trapping data
joined <- left_join(
  x = product$mam_trapNight_nodups,
  y = select(ibutton_lookup, -endDate),
  by = c("nightuid", "trapCoordinate", "tagID")
) |>
  mutate(
    collectDate = ymd(collectDate),
    year = year(collectDate),
    month = month(collectDate)
  )

joined |>
  distinct(trapCoordinate, collectDate, tagID) |>
  dim()

# save this joined data


####
tmp <- read.csv("data/mammalcapturedatawithibuttons.csv")
