## The functions contained within this R script are meant to assist with
## Plotting piecewiseSEM::psem() output.

# TODO: Put this in a package

## ---- Required packages ----
library(dplyr)
library(DiagrammeR)

## ---- Build list of nodes and edges ----
#' Get a list of elements required to plot the results of a psem
#'
#' @param coef_tab a data frame containing at three columns with names given
#' by \code{from_col}, \code{to_col}, and \code{val_col}.
#' @param from_col name of the column identifying edge sources (predictors).
#' This column will be renamed to "From" in the output.
#' @param to_col name of the column identifying edge destinations (responses).
#' This column will be renamed to "To" in the output.
#' @param val_col name of the (numeric) column containing edge values
#' (e.g, coefficients). This column will be renamed to "Val" in the output.
#'
#' @returns a list containing a \code{nodes} and \code{edges} table.
#'
#' \code{nodes} will contain the node \code{name} and numeric \code{id} columns.
#'
#' \code{edges} will contain all the original columns in coef_tab (rearranged)
#' plus two new columns corresponding to node numbers for the \code{from_col}
#' and \code{to_col}.
#'
#' @seealso [DiagrammeR::create_graph()], [piecewiseSEM::psem()]
#'
#' @export
#'
#' @examples
#'
#' library(piecewiseSEM)
#' mod <- psem(
#'  lm(rich ~ cover + age, data = keeley),
#'  lm(cover ~ firesev + age, data = keeley),
#'  lm(firesev ~ age, data = keeley),
#'  data = keeley
#' )
#'
#' mod_fit <- summary(mod)
#'
#' (nodes_edges <- build_nodes_edges(mod_fit$coefficients))
#'
build_nodes_edges <- function(
    coef_tab, from_col = "Predictor", to_col = "Response",
    val_col = "Std.Estimate"
  ){
  # ensure the table is a data frame
  coef_tab <- data.frame(coef_tab)

  # ensure all necessary columns are present, with error handling
  if (!all(c(from_col, to_col, val_col) %in% colnames(coef_tab))){
    stop(paste0(
      "all of columns [", paste(from_col, to_col, val_col),
      "] not present in coef_tab."
    ))
  }

  # extract the unique variables
  vrs <- unique(c(coef_tab[[from_col]], coef_tab[[to_col]]))

  # combine variables into a table, with numeric IDs
  nodes_tab <- data.frame(
    name = vrs
  ) %>% mutate(id = row_number())

  # Match the from and to columns to the node table row
  from_match <- match(coef_tab[[from_col]], nodes_tab[["name"]])
  to_match <- match(coef_tab[[to_col]], nodes_tab[["name"]])
  # Assign the new columns
  coef_tab[["From_id"]] <- nodes_tab$id[from_match]
  coef_tab[["To_id"]] <- nodes_tab$id[to_match]

  # rearrange the columns
  coef_tab <- coef_tab %>%
    rename("From" = from_col, "To" = to_col, "Val" = val_col) %>%
    relocate(c(To, To_id, From, From_id, Val), .before = 0)

  # return list comprised of the nodes and edges tables
  list(nodes = nodes_tab, edges = coef_tab)
}

## ---- Add aesthetics to nodes and edges tables ----
#' Add node aesthetics to a node table using functions
#'
#' @description
#' lifecycle::badge("experimental")
#'
#' @seealso [DiagrammeR::node_aes]
add_nodes_aes <- function(){
  warning("This function is not yet implemented.")
}

#' Add edge aesthetics to a node table using functions
#'
#' @description
#' lifecycle::badge("experimental")
#'
#'
#' @seealso [DiagrammeR::edge_aes]
#'
#' @inherit build_nodes_edges examples
#' @examples
#'
#' ## Example of desired behavior...
#'
#' nodes_edges$edges <- nodes_edges$edges %>%
#'   mutate(
#'     color = if_else(Val >= 0, "cornflowerblue", "orange")
#'   )
add_edges_aes <- function(){
  warning("This function is not yet implemented.")
## Unused example trying to color edges by Std.Estimate
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
}

## ---- Build PATH diagram from nodes and edges list ----
#' Build a (basic) diagram from plot frames
#'
#' @param nodes_edges a "nodes_edges" list containing a \code{nodes} table and
#' a \code{edges} table.
#' @param render,attr_theme,name,directed,write_backups,display_msgs arguments
#' passed to [DiagrammeR::create_graph].
#'
#' @returns either a \code{DiagrammeR} graph object, or a rendered graph
#'
#' @seealso [DiagrammeR::create_graph], [DiagrammeR::render_graph]
#'
#' @export
#'
#' @inherit build_nodes_edges examples
#' @examples
#'
#' # render with defaults
#' build_psem_plot(nodes_edges, render = TRUE)
#'
#' # save instead
#' plot_spec <- build_psem_plot(nodes_edges)
#'
#' library(DiagrammeR)
#'
#' # render with altered global attributes
#' plot_spec %>%
#'   set_node_attrs("shape", "rectangle") %>%
#'   set_edge_attrs("color", "black") %>%
#'   render_graph()
#'
#' ## add colored edges (directly in edges frame)
#'
#' # copy the nodes_edges list
#' new_list <- nodes_edges
#'
#' # add color and size to the edges table...
#' new_list$edges <- new_list$edges %>%
#'   mutate(
#'     color = if_else(Val >= 0, "cornflowerblue", "orange"),
#'     penwidth = scales::rescale(abs(Val), to = c(1, 9))
#'   )
#'
#' # render the plot (with color!)
#' build_psem_plot(new_list, render = TRUE)
#'
build_psem_plot <- function(
    nodes_edges, from = "Predictor", to = "Response",
    render = FALSE,
    attr_theme = "bt", name = "pSEM graph",
    directed = TRUE, write_backups = FALSE, display_msgs = FALSE
  ){

  # ensure that nodes_edges is a length 2 list
  stopifnot(is.list(nodes_edges) & length(nodes_edges) == 2)
  # and that its elements are called "nodes" and "edges"
  stopifnot(all(c("nodes", "edges") %in% names(nodes_edges)))
  ## TODO: the above could be solved by assigning a class in build_nodes_edges()

  # build the basic plot
  path_plot <- create_graph(
    attr_theme = attr_theme, graph_name = name, directed = directed,
    write_backups = write_backups, display_msgs = display_msgs
  ) %>%
    add_nodes_from_table(nodes_edges[["nodes"]], label_col = "name") %>%
    add_edges_from_table(
      nodes_edges[["edges"]], from_col = "From", to_col = "To",
      from_to_map = "label"
    )

  if (render) {
    # Render the plot, if requested
    render_graph(path_plot)
  } else {
    # otherwise, return the graph object
    path_plot
  }
}

## ---- Build PATH diagram directly from psem output ----
#' Plot a psem summary object
#'
#' @param psem_summary "summary.psem" object
#'
#' @description
#' lifecycle::badges("experimental")
#'
#' @seealso [piecewiseSEM:::summary.psem]
#'
plot_psem <- function(
    psem_summary
    # coef_tab, render = TRUE, attr_theme = "bt",
    # node_width = 1, node_height = 0.5,
    # node_fontsize = 16, node_fontcolor = "black", node_bordercol = "black"
  ){
  warning("This function is not yet implemented.")
  # # get node and edges list
  # L <- build_nodes_edges(coef_tab)
  #
  # # build the basic plot
  # base <- build_psem_plot(base, render = FALSE)
  #
  # # add some
  # base %>%
  #   set_node_attrs(width, node_width) %>%
  #   set_node_attrs(height, node_height) %>%
  #   set_node_attrs()
}

## ---- Extract coefficients along a SEM path ----
#' Get a vector of the coefficients along a path
#'
#' @param p a vector of integers defining the path
#' @param edges an edges table
#' @param nodes a nodes table
#'
#' @returns a vector of effects.
#'
#' @inherit build_psem_plot examples
#' @examples
#'
#' # numeric values for "firesev -> cover -> rich" path
#' pth = c("firesev", "cover", "rich")
#' pth_num = nodes_edges$nodes$id[match(pth, nodes_edges$nodes$name)]
#'
#' # Coefficients along a specific our path
#' get_coefs_along_path(pth_num, nodes_edges$edges, nodes_edges$nodes)
#'
#' ## get all the unique paths from "age" (2) to "rich"  (4) in the model
#' age_rich_paths <- DiagrammeR::get_paths(from = 2, to = 4, graph = plot_spec)
#'
#' ## get the corresponding coefficients
#' (coef_path_list <- lapply(
#'   age_rich_paths,
#'   function(x){get_coefs_along_path(x, nodes_edges$edges, nodes_edges$nodes)}
#' ))
get_coefs_along_path <- function(p, edges, nodes){
  # early return for a missing path
  if (length(p) == 1 | all(is.na(p))){
    return(NA)
  }

  # convert the path numbers into names
  v <- nodes$name[p]

  # direct effect
  if (length(p) == 2) {
    ## return the direct effect
    edges %>%
      filter(From == v[1], To == v[2]) %>%
      pull(Val) %>% return()
  }

  # indirect effects

  ## join into a lag table
  tab <- cbind(from = head(v, -1), to = tail(v, -1))

  ## go through all the rows of the table and find the matching coefficient
  apply(
    tab, 1, function(x) {
      pull(filter(edges, From == x[1], To == x[2]), Val)
    }
  ) %>% return()
}

## ---- Calculate cumulative effects ----
#' Calculate the cumulative effects (indirect + direct) of a response
#' on a predictor.
#'
#' #' @description
#' lifecycle::badges("experimental")
#'
#' @returns
#' @export
#'
#' @inherit get_coefs_along_path examples
#' @examples
#'
#' # For the example given above...
#'
#' direct = coef_path_list[[1]] # the element with only one coefficient
#' prods = unlist(lapply(coef_path_list, prod)) # products of each path
#' total = sum(prods)
#'
#' # effect breakdown for age on rich:
#' c(direct = direct, indirect = total - direct, total = total)
accumulate_path_coefs <- function(){
  warning("This function is not yet implemented. See example.")
}
