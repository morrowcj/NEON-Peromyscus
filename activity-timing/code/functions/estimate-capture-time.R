library(dplyr)
library(ggplot2)

ac1 <- function(z, type = "covariance") {
  acf(z, plot = FALSE, type = type)$acf[2]
}

acmean <- function(z){
  mean(acf(z, plot = FALSE)$acf[2:4], na.rm = TRUE)
}

regcoef <- function(a, b){
  fm = suppressWarnings(lm(a ~ b))
  smr = suppressWarnings(summary(fm))
  smr$r.squared
}

# summary function
my_summary <- function(x, na.rm = TRUE){
  out = c(
    summary(x, na.rm = na.rm) |> as.vector(),
    var(x, na.rm = na.rm)
  )
  names(out) = c("min", "Q1", "med", "mean", "Q3", "max", "var")
  return(out)
}

#' Estimate captures from iButton pairs
#'
#' @param x first time series of temperature data
#' @param y second time series of temperature data
#' @param time (optional) vector of times corresponding to the time series
#' @param k number of time points used in rolling average smoothing (if > 1)
#' @param threshold the minimum difference between x and y that might indicate
#' a capture
#' @param sequential number of times in a row that threshold must be met before
#' a capture is considered.
#'
#' @returns a data frame with rows for each possible capture and columns for
#' the start, end, and duration of the capture sequence.
#' @export
#'
#' @examples
#' x <- rnorm(100, mean = 20, sd = 5)
#' diffs <- c(rep(0, 20), rep(2, 18), rep(0, 12), rep(3, 20), rep(0, 30))
#' y <- x + diffs
#' tmp <- estimate_capture_time(x, y)
#' tmp |> select(segmentID, pick)
estimate_capture_time <- function(
    x, y, time, k = 1, threshold = 1, sequential = 10
    # plot_dir = NULL, file_name = "capture_time_estimates.jpg", 
    # create_dir = TRUE, debug = TRUE
) {
  
  require(dplyr)
  
  # ensure equal length
  stopifnot(length(x) == length(y))
  
  # handle missing time
  if (missing(time)) {
    time <- 1:length(x)
  }
  
  # # update the threshold
  diffs = abs(x - y)
  quant = quantile(diffs, 0.005, na.rm = TRUE)
  threshold = max(threshold, quant, na.rm = TRUE)
  
  # check if the x is generally greater than y
  is_ordered_right = mean(x, na.rm = TRUE) >= mean(y, na.rm = TRUE)
  
  # set it up such that the larger variable is x.
  if (is_ordered_right) {
    fixed.x = x
    fixed.y = y
  } else {
    fixed.x = y
    fixed.y = x
  }
  
  # combine the data
  df <- dplyr::tibble(
    og.x = fixed.x,
    og.y = fixed.y,
    x = zoo::rollmean(og.x, k, na.pad = TRUE),
    y = zoo::rollmean(og.y, k, na.pad = TRUE),
    rat = x/y,
    time = time
  ) |> 
    dplyr::mutate(
      og.diffs = og.x - og.y,
      diff_diffs = c(NA, diff(og.diffs)),
      diffs = x - y,
      abs_diffs = abs(diffs),
      rat_met = rat >= 1.05,
      diff_met = abs_diffs >= threshold # & rat_met
    )
  
  # get summary statistics for all the temperature variables
  df_summary <- df |> reframe(
    across(c(og.x:y, og.diffs:diffs), ~ my_summary(na.omit(.x)))
  ) |> mutate(variable = c("min", "Q1", "med", "mean", "Q3", "max", "var")) |> 
    relocate(variable, .before = 1)
  
  # get the different segments of the time series
  segment_tab <- df |> 
    pull(diff_met) |> # extract the boolean column
    rle() |> # perform run len analysis
    unclass() |> # remove the RLE class for manipulation
    data.frame() |> # convert from list to data frame
    tibble() |>  # then a tibble for convenience
    rename("len" = "lengths", "thresh_met" = "values") |> 
    mutate(
      ID = row_number(), # label the segments
      start = 1 + cumsum(len) - len,
      end = start + len - 1,
      start_time = time[start],
      end_time = time[end],
      flag = thresh_met & len >= sequential
    ) |>  
    relocate(ID, .before = 1)
  
  # get a subset of the segments corresponding to possible captures
  possible_caps <- segment_tab |> 
    filter(flag) |> 
    select(-"flag", -"thresh_met")
  
  # label each time step with the information from segment tab
  step_tab <- segment_tab |> 
    rowwise() |>
    reframe(
      segmentID = ID, segmentLength = len, 
      time_step = seq(start, end), flagged_seg = flag
    ) |> mutate(time = time[time_step])
  
  # merge the time step info with the data
  new_df <- left_join(df, step_tab, by = "time", relationship = "many-to-many")
  
  # summarize by the segments
  group_summary <- new_df |> 
    group_by(segmentID, flagged_seg, segmentLength) |> 
    summarize(
      # how much does the x for this segment differ from the overall x average?
      segx_above_avg = df_summary |> filter(variable == "mean") |> pull(og.x),
      segx_above_avg = segx_above_avg - mean(og.x), # looking for most positive
      # average diffs for this segment
      avg_seg_diffs = mean(og.diffs),
      # how much do the diff for this segment differ from overall avg diffs?
      seg_rel_diff_avg = df_summary |> filter(variable == "mean") |> pull(og.diffs),
      seg_rel_diff_avg = seg_rel_diff_avg/mean(og.diffs), # looking for most positive
      # what is the variance for this segment?
      seg_diff_var = var(og.diffs, na.rm = TRUE),
      # realtive difference
      seg_prop_diff = mean((x - y) / y, na.rm = TRUE),
      # temporal autocorrelation
      autocor_x = ac1(og.x, "correlation"),
      autocor_y = ac1(og.y, "correlation"),
      autocor_ratio = autocor_x / autocor_y,
      avg_autocor_x = acmean(og.x),
      avg_autocor_y = acmean(og.y),
      avg_autocor_ratio = avg_autocor_x / avg_autocor_y,
      # roughness
      rougness_x = 2 * (1 - (ac1(og.x, "covariance") / var(og.x))),
      rougness_y = 2 * (1 - (ac1(og.y, "covariance") / var(og.y))),
      roughness_ratio = rougness_x/rougness_y,
      # regression coefficient
      regression_coef_xy = regcoef(og.x, og.y),
      # coefficient of variation
      CV_x = sd(diff(og.x)) / abs(mean(diff(og.x))),
      CV_y = sd(diff(og.y)) / abs(mean(diff(og.y))),
      CV_ratio = CV_x / CV_y,
      .groups = "drop") |> 
    left_join(
      segment_tab |> select(-"thresh_met"), 
      by = c("segmentID" = "ID", "segmentLength" = "len", "flagged_seg" = "flag")
    ) |>
    group_by(flagged_seg) |> 
    mutate(
      prop_diff_rank = dense_rank(desc(seg_prop_diff)), # prefer positive diffs
      length_rank = dense_rank(desc(segmentLength)), # prefer longer lengths
      # rxdev = dense_rank(desc(segx_above_avg)), # prefer greater deviation from mean
      # rdiffs = dense_rank(desc(avg_seg_diffs)), # prefer greater x
      # rdifdev = dense_rank(desc(seg_rel_diff_avg)), # prefer greater differences
      diff_var_rank = dense_rank(desc(seg_diff_var)), # prefer greater variance
      autocor_ratio_rank = dense_rank(avg_autocor_ratio), # prefer lower autocor in x
      roughness_ratio_rank = dense_rank(desc(roughness_ratio)), # prefer higher roughness in x
      regression_coef_rank = dense_rank(regression_coef_xy), # prefer a low regression coefficient
      CV_ratio_rank = dense_rank(desc(CV_ratio)), # prefer higher variance in x
      time_rank = rank(start_time), # prefer earlier times
      # TODO: should weight towards the middle?
    ) |> 
    ungroup() |> 
    rowwise() |> 
    mutate(
      rscore = weighted.mean(
        x = c(length_rank, prop_diff_rank, roughness_ratio_rank, 
              regression_coef_rank, diff_var_rank, CV_ratio_rank, 
              time_rank, autocor_ratio_rank),
        w = c(3,           2,              2,         
              1.5,                  1,             1,
              1,         1)
      )
    )
  
  flagged_groups <- group_summary |> 
    filter(flagged_seg) 
  
  if (nrow(flagged_groups) > 0) {
    ranked_caps <- flagged_groups |> 
      ungroup() |> 
      arrange(rscore, desc(segmentLength), start_time) |> 
      mutate(pick = c(TRUE, rep(FALSE, times = nrow(flagged_groups) - 1))) |> 
      arrange(start_time)
  } else {
    ranked_caps <- flagged_groups |> 
      ungroup() |> 
      mutate(pick = logical())
  }

  # return possible the capture table
  # return(ranked_caps |> filter(pick) |> pull(start_time)) 
  
  attr(ranked_caps, "data") <- new_df |> 
    select(
      time = time, temp_in_roll = x, temp_out_roll = y, temp_in = "og.x", 
      temp_out = "og.y"
    )
  
  return(ranked_caps |> arrange(start_time))
}

plot_captures <- function(){
  # plot
  if (make_plot | debug) {
    line_size = 2/3
    fade_alpha = 1/2
    fig <- ggplot(new_df %>% filter(complete.cases(.)), aes(x = time)) + 
      geom_rect(
        data = ranked_caps, inherit.aes = FALSE, alpha = 0.1, 
        fill = ifelse(ranked_caps$pick, "purple", "grey80"),
        aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf)
      ) + 
      geom_vline(data = ranked_caps, aes(xintercept = start_time), 
                 col = "grey50", linetype = "dashed") + 
      geom_line(aes(y = y, col = "y"), linewidth = line_size) + 
      geom_line(aes(y = og.y, col = "y"), alpha = fade_alpha) + 
      geom_line(aes(y = x, col = "x"), linewidth = line_size) + 
      geom_line(aes(y = og.x, col = "x"), alpha = fade_alpha) + 
      scale_color_manual(name = NULL, labels = c("in", "out"),
                         values = c("orange", "cornflowerblue")) +
      labs(x = "Time", y = "Temperature") +
      theme_bw()
    
    if (debug) {plot(fig)}
    
    # print it
    if (make_plot) {
      ggsave(filename = plot_path, plot = fig, width = 6, height = 0.8*6)
    }   
  }}
# 
# --- OLD ----
#' #' Estimate capture times for multiple traps
#' #'
#' #' @param temp a vector of temperature data
#' #' @param time a vector of time points
#' #' @param PID a vector of pair IDs
#' #' @param IID a vector of individual IDs
#' #' @param ... additional arguments passed to estimate_capture_time
#' #' @param pb should a progress bar track progress?
#' #'
#' #' @returns all estimated capture events for each PID
#' #' @export
#' #'
#' #' @examples
#' #' x1 <- rnorm(100, mean = 20, sd = 5)
#' #' y1 <- x1
#' #' add_inx1 <- c(18:30, 55:60, 63:80)
#' #' y1[add_inx1] <- y1[add_inx1] + 3
#' #'
#' #' x2 <- rnorm(100, mean = 20, sd = 5)
#' #' y2 <- x2
#' #' add_inx2 <- c(8:20, 40:55, 60:85)
#' #' y2[add_inx2] <- y2[add_inx2] + 3
#' #'
#' #' estimate_capture_data(
#' #'   temp = c(x1, y1, x2, y2),
#' #'   time = rep(1:100, times = 4),
#' #'   IID = rep(1:4, each = 100),
#' #'   PID = rep(1:2, each = 200)
#' #' )
#' estimate_capture_data <- function(temp, time, PID, IID, ...) {
#'   # check dimensions
#'   stopifnot(length(temp) == length(time) &
#'               length(temp) == length(PID) &
#'               length(temp) == length(IID))
#'   
#'   # combine into a table
#'   df <- data.frame(temp = temp, time = time, PID = PID, IID = IID) |>
#'     arrange(PID, time)
#'   
#'   # empty object to track unpaired data
#'   incomplete_pairs <- {}
#'   
#'   # estimate captures for each pair
#'   captures <- lapply(unique(PID), FUN = function(p) {
#'     # for (p in unique(PID)) {
#'     # get data for the pair only
#'     df_p <- df |> filter(PID == p)
#'     
#'     # split into two
#'     split_df <- df_p |>
#'       group_by(IID) |>
#'       group_split()
#'     
#'     if (length(split_df) == 2) {
#'       # calculate and return the captures
#'       caps <- estimate_capture_time(
#'         x = split_df[[1]]$temp,
#'         y = split_df[[2]]$temp,
#'         time = split_df[[1]]$time,
#'         # ...
#'       )
#'       caps$PID <- p
#'       caps
#'     } else {
#'       # add to the incomplete pairs
#'       incomplete_pairs <- c(incomplete_pairs, p)
#'     }
#'   }) |> bind_rows() # combine into a single table
#'   
#'   # output the capture table
#'   attr(captures, "incomplete_pairs") <- incomplete_pairs
#'   captures
#' }
