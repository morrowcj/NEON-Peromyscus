#' Function to run/cache objects
#'
#' @param expr R code to evaluate
#' @param rds_path path to save object
#' @param force should the object be overwritten?
#'
#' @returns
#' @export
#'
#' @examples
run_cache <- function(expr, rds_path, force = FALSE){
  dr = dirname(rds_path)
  if (dr != "." && !dir.exists(dr)) {
    dir.create(dr, recursive = TRUE)
  }
  
  if (force || !file.exists(rds_path)) {
    x = eval(expr)
    saveRDS(x, rds_path)
  } else {
    x = readRDS(rds_path)
  }
  return(x)
}

#' Function to plot/cache ggplot objects
#'
#' @param ggp ggplot code
#' @param plot_path path to save plot
#' @param force should the plot be overwritten?
#' @param ... additional arguments passed to ggsave
#'
#' @returns
#' @export
#'
#' @examples
plot_cache <- function(ggp, plot_path, force = FALSE, ...){
  dr = dirname(plot_path)
  if (dr != "." && !dir.exists(dr)) {
    dir.create(dr, recursive = TRUE)
  }
  
  if (force || !file.exists(plot_path)) {
    x = eval(ggp)
    ggsave(plot_path, x, ...)
    return(x)
  } else {
    graphic_path <- R.utils::getAbsolutePath(plot_path)
    knitr::include_graphics(graphic_path, rel_path = FALSE, error = FALSE)
  }
}