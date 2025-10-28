# load libraries
library(tidyverse)
library(lubridate)

# Load the function that estimates capture time from a pair of temperature
## streams
source("R/functions/estimate-capture-time.R")

## ---- Temperature data ----

# Read in ibutton data
df <- read.csv(
  "data/ibuttons/raw-temperatures/HARV_07_01_100s_downloaded_7_15.csv"
  # "data/ibuttons/raw-temperatures/"
) %>% tibble()
# This example file has 2048 rows (plus a header) and 197 columns.
## The first column represents the datetime of an observation and the
## remaining columns represent observations from indivivdual ibuttons

# get the time column in the proper format
datetimes <- df %>%
  pull("Time") %>%  # extract the time column
  ymd_hms(tz = "UTC") # convert to datetime format

# data frame (tibble) of temperatures
temp_df <- df %>%
  select(-Time) %>% # exclude the time column
  tibble() # convert to a tibble

# check that all temperature columns are numbers and not characters:
lapply(temp_df, is.numeric) %>% # apply is.numeric to all columns
  unlist() %>% # convert from vector to list
  all() %>% # are all elements true?
  stopifnot() # throw an error if not

# get a cleaned list of ibutton serial numbers from the column names.
## the cleaining is usually required because of artifacts from the download process
SNs <- colnames(temp_df) %>%
  gsub("\\..*", "", .) %>%  # remove anything following a decimal
  gsub("\\*$", "", .) %>% # remove any * at the end of a SN
  gsub("^X", "", .) %>% # remove any leading "X"
  factor() # convert to a factor

# replace the column names with the cleaned names
names(temp_df) <- SNs

## ---- iButton pair data ----

# read in, and clean, ibutton metadata (including pairs)
meta_df <- read.csv(
  "data/ibuttons/raw-metadata/2024 - UW_Madison_Fob_Serial_Numbers.csv",
  na.strings = c(NA, "")
) %>%
  tibble() %>% # tibble format
  set_names("pair", "IO", "name", "new_pair", "SN", "status", "ok", "notes") %>% # rename the columns
  mutate(
    pair = factor(pair), SN = factor(SN), # convert to factors
    IO = gsub("_", "", IO), # remove leading "_"
    ok = (!is.na(ok) & ok == "x"), # convert to TRUE/FALSE
    is_repair = FALSE, # column indicating if the button was put in a new pair (placeholder)
    sci_ntn_sn = grepl("\\+|\\-", SN) # flag if excel converted the SN to scientific notation.
  )

# Create a table for the new pairs as well
new_meta_df <- meta_df %>%
  filter(!is.na(new_pair)) |>
  mutate(
    status = paste("previously", name),
    pair = gsub("(.*)_(.*)", "\\1", new_pair) %>%  factor(), # element before "_"
    IO = gsub("(.*)_(.*)", "\\2", new_pair) %>%  factor(), # element after "_"
    name = new_pair,
    new_pair = NA,
    is_repair = TRUE
  )

# match each SN from the temperature data to its pair
## NAs meen that no match was found.
SN_pair_row <- match(SNs, meta_df$SN)

# Group the buttons by pairs
pair_tab <- meta_df %>%
  select(pair, IO, SN) %>%
  filter(SN %in% .env$SNs) %>% # match SN to those in the environment variable (.env) SNs
  group_by(pair) %>%
  pivot_wider(names_from = IO, values_from = SN)

# get a list of any missing buttons
missing_ins <- which(is.na(pair_tab$In))
missing_outs <- which(is.na(pair_tab$Out))

# warn the user if there are missing pairs
if (length(missing_ins) > 0 & length(missing_outs) > 0) {
  warning(
    "some ibuttons are missing their pairs. Check for them in 'new_meta_df'."
  )

  # build a pair tab from any missing pairs
  new_pair_tab <- new_meta_df %>%
    select(pair, IO, SN) %>%
    filter(SN %in% c(.env$SNs[missing_ins], .env$SNs[missing_outs])) %>%
    group_by(pair) %>%
    pivot_wider(names_from = IO, values_from = SN)

  # throw an error if there are still missing pairs (fix manually)
  stopifnot(
    !any(is.na(new_pair_tab$In) || is.na(new_pair_tab$Out))
  )

  # combine the two
  pair_tab %>%
    filter(complete.cases(.)) %>%  # remove rows with NAs
    bind_rows(new_pair_tab) # stack onto the new pairs

}

## ---- Estimate activity times ----

# divide the date times into chunks based on trappings
## We will only search for captures within this window
first_day = ymd("2024-07-02")
last_day = ymd("2024-08-08")
trap_start = hms("18:00:00") # starting time on a trap night
trap_end = hms("08:00:00") # ending time on a trap night (next day)

out_list = list()

# loop through each pair
for (p in seq_len(nrow(pair_tab))) {

  # get SNs for this pair
  in_SN = pair_tab$In[p]
  out_SN = pair_tab$Out[p]

  # build a table of time series of temperatures for this pair
  tmp <- tibble(
    pair = p,
    in_temp = temp_df[[in_SN]], # in button temps
    out_temp = temp_df[[out_SN]], # out button temps
    datetime = datetimes, # datetimes above
    date = date(datetime) # date
  ) %>% filter(
    date >= first_day, date <= last_day,
    datetime <= date + trap_end | # keep times before trapping end...
      datetime >= date + trap_start # or before start...
  ) %>%
    mutate(
      phase = if_else(datetime >= date + hms("12:00:00"), "evening", "morning"),
      trapnight = time_length(datetime - lag(datetime), unit = "min"), # time since last measurment
      trapnight = trapnight > (8*60), # new event if longer than 8 hours
      trapnight = cumsum(if_else(is.na(trapnight), 0, trapnight)), # add up new trapnights
    )

  # empty table to fill
  out <- tibble()

  # loop through each trapnight
  for (n in unique(tmp$trapnight)) {
    dat <- tmp %>% filter(trapnight == n)

    # estimate possile capure times for this trapnight
    ## parameter values can be tweaked.
    ## see the function documentation for more details
    results <- estimate_capture_time(
      x = dat$in_temp, y = dat$out_temp, time = dat$datetime,
      k = 10, # lag-10 moving average
      threshold = 1, # looking for 1-degree differences
      sequential = 10 # looking for 10 times in a row above threshold
    ) %>%
      mutate(trapnight = n, pair = p)
    ## Note that the result object is a table with one row for each **possible**
    ## trap start (i.e., all criteria may be met multiple times).
    ## The "pick" column indicates which of these possibilities is most likely
    ## In most cases, you can just select this row, but it is worth evaluating them.
    ## start_time (where pick=TRUE) is the estimated capture time.

    # combine the output
    out <- bind_rows(out, results)
  }

  # add it to the list
  out_list[[p]] <- out
}


# Here's an example figure
tmp %>%
  ggplot(aes(x = datetime)) +
  facet_wrap(~trapnight, scales = "free", labeller = "label_both") +
  geom_line(aes(y = in_temp, col = "IN")) +
  geom_line(aes(y = out_temp, col = "OUT")) +
  # add rectangle around the window
  geom_rect(
    data = out, inherit.aes = FALSE,
    aes(
      xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf,
      fill = pick
    ),
    alpha = 0.2
  ) +
  geom_vline(
    data = out,
    aes(xintercept = start_time, linetype = pick), col = "black"
  ) +
  labs(y = "Temperature (C)", x = "Time", color = "button")
