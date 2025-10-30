library(dplyr)
library(DiagrammeR)

## ---- Functions ----

#' Get a list of elements required to plot the results of a psem
#'
#' @param coef_tab a data frame containing at least "Response", "Predictor",
#' "Std.Estimate", and "P.Value" columns (as obtained from
#' \code{summary(psem(...))$coefficients}). Other columns matching an
#' \code{edge_aes()} value will be included.
#'
#' @returns a list containing a \code{nodes} and \code{edges} table.
#'
#' @seealso [DiagrammeR::create_graph(), piecewiseSEM::psem()]
#'
frameplot_psem <- function(coef_tab){

  coef_tab <- data.frame(coef_tab)

  # ensure all necessary columns are present
  stopifnot(all(
    c("Response", "Predictor", "Std.Estimate", "P.Value") %in% colnames(coef_tab)
  ))

  # extract the unique variables
  vrs <- unique(c(coef_tab$Response, coef_tab$Predictor))

  # combine variables into a table, with numeric IDs
  nodes_tab <- data.frame(
    name = vrs
  ) %>% mutate(id = row_number())

  list(nodes = nodes_tab, edges = coef_tab)

}

# effect_color_fun <- function(
#     plotframes, neg_col = "orange", pos_col = "cornflowerblue", null_col = "grey50"
#   ) {
#   edge_df = plotframes$edges %>%
#     mutate(
#       positive = Std.Estimate >= 0,
#       rescaled_effect = if_else(
#         condition = positive,
#         true = scales::rescale(Std.Estimate, from = c(0, max(Std.Estimate)), to = c(0.5, 1)),
#         false = scales::rescale(Std.Estimate, from = c(min(Std.Estimate), 0), to = c(0, 0.5))
#       ),
#       col = scales::div_gradient_pal(low = neg_col, mid = null_col, high = pos_col)(rescaled_effect)
#     )
#
#
#   # neg_col_fun <- colorRamp(c(null_col, neg_col))
#   # pos_col_fun <- colorRamp(c(null_col, pos_col))
# }

#' Title
#'
#' @param plotframes
#' @param render
#' @param attr_theme
#' @param name
#' @param directed
#' @param write_backups
#' @param display_msgs
#'
#' @returns
#' @export
#'
#' @examples
build_psem_plot <- function(
    plotframes, render = FALSE, attr_theme = "bt", name = "pSEM graph",
    directed = TRUE, write_backups = FALSE, display_msgs = FALSE
  ){

  # ensure that plotframes is a length 2 list
  stopifnot(is.list(plotframes) & length(plotframes) == 2)
  # and that its elements are called "nodes" and "edges"
  stopifnot(all(c("nodes", "edges") %in% names(plotframes)))

  # build the basic plot
  psem_plot <- create_graph(
    attr_theme = attr_theme, graph_name = name, directed = directed,
    write_backups = write_backups, display_msgs = display_msgs
  ) %>%
    add_nodes_from_table(plotframes[["nodes"]], label_col = "name") %>%
    add_edges_from_table(
      plotframes[["edges"]], from_col = "Predictor", to_col = "Response",
      from_to_map = "label"
    )

  if (render) {
    render_graph(psem_plot)
  } else {
    psem_plot
  }

}

#' Title
#'
#' @param coef_tab
#' @param render
#' @param attr_theme
#'
#' @returns
#' @export
#'
#' @examples
# plot_psem <- function(
#     coef_tab, render = TRUE, attr_theme = "bt",
#     node_width = 1, node_height = 0.5,
#     node_fontsize = 16, node_fontcolor = "black", node_bordercol = "black"
#   ){
#   # get node and edges list
#   L <- frameplot_psem(coef_tab)
#
#   # build the basic plot
#   base <- build_psem_plot(base, render = FALSE)
#
#   # add some
#   base %>%
#     set_node_attrs(width, node_width) %>%
#     set_node_attrs(height, node_height) %>%
#     set_node_attrs()
#
# }

#' Get a vector of the coefficients along a path
#'
#' @param p a vector of integers defining the path
#' @param edges an edges table
#' @param nodes a nodes table
#'
#' @returns a vector of effects.
get_coefs_along_path <- function(p, edges, nodes){
  # early return for a missing path
  if (length(p) == 1 | all(is.na(p))){
    return(NA)
  }

  # convert the path numbers into names
  v <- nodes$name[p]

  ## direct effect
  if (length(p) == 2) {
    # return the direct effect
    edges %>%
      filter(Predictor == v[1], Response == v[2]) %>%
      pull(Std.Estimate) %>% return()
  }

  ## indirect effects

  # join into a lag table
  tab <- cbind(from = head(v, -1), to = tail(v, -1))

  # go through all the rows of the table and find the matching coefficient
  apply(
    tab, 1, function(x) {
      pull(filter(edges, Predictor == x[1], Response == x[2]), Std.Estimate)
    }
  ) %>% return()
}

## ---- Example data set ----

# Load an example SEM model fit from summary(psem(...))
example_SEM <- readRDS(
  "infection-modeling/data/model-objects/simplified-SEM-objects.rds"
)$sem_mod$fit

# Extract the coefficient table
example_df <- example_SEM$coefficients
pf <- frameplot_psem(example_df)
base_grph <- build_psem_plot(pf)


