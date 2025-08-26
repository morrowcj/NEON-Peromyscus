# function to plot a piecewise SEM model

library(tidyverse)
library(DiagrammeR)

mod = readRDS(
  "infection-modeling/data/model-objects/selected-burden-SEM_maximal-cases.rds"
)

tab <- mod$coefficients

unique(c(tab$Response, tab$Predictor))

recode_tab <- tribble(
  ~old, ~new,
  "sex_male", "male",
  "sex_mature", "mature",
  "weight", "weight",
  "sex_male:weight", "male:weight",
  "sex_male:sex_mature", "male:mature",
  "ticks_attached", "ticks", 
  "expr_PC2", "tol PC",
  "expr_PC1", "res PC",
  "Bb_burden", "burden",
  "Bb_infected", "infected",
  "cap_prop_night", "captime",
  "weighted_trapability", "trapable",
  "weighted_trap_diversity", "trapdiv",
  "avg_move_dist", "movement",
  "wthr_PC1", "wthr PC1",
  "wthr_PC2", "wthr PC2",
  "clim_PC1", "clim PC1",
  "clim_PC2", "clim PC2"
)

#' Replace values from key-value pairs
#'
#' @param x vector of values
#' @param old vector of values to be replaced
#' @param new vector of values to replace with
#'
#' @examples
#' key_val_recode(
#'   x = c("ham", "eggs", "eggs", "cheese"), 
#'   old = c("ham", "eggs"),
#'   new = c("green ham", "green eggs")
#' )
key_val_recode <- function(x, old, new){
  inx = match(x, old)
  if_else(condition = is.na(inx), true = x, false = new[inx])
}

#' Group p-values by significance
#'
#' @param p vector of p-values
#' @param breaks vector of "breaks" for significant p-values passed to 
#' \code{cut}
#'
#' @examples
#' (g <- cut_pvals(p = c(0.18, 0.001, 0.08, 0.23, 0.01, 0.008, 0.2)))
#' as.numeric(g)
cut_pvals <- function(p, breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.15)) {
  labs = paste0("<", breaks[2:length(breaks)])
  out = cut(p, breaks, include.lowest = TRUE, labels = labs)
}

#' format psem coefficient table
#'
#' @param mod psem model
#' @param cols column names
#' @param recode (optional) recode table with two columns
#'
#' @examples
#' mod = readRDS(
#'   file.path(
#'     "infection-modeling/data/model-objects",
#'     "selected-burden-SEM_maximal-cases.rds"
#'   )
#' )
#' 
#' format_psem_coefs(mod)
format_psem_coefs <- function(
    mod, 
    cols = c(
      "response", "predictor", "coefficient", 
      "SE", "DF", "crit", "P", 
      "effect", "star"
    ), 
    recode = recode_tab,
    p_breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.15)
){
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  # replace the names, convert to tibble
  tab <- mod$coefficients %>% data.frame() %>% setNames(cols) %>% tibble()
  # recode response and predictor columns
  if (!is.null(recode_tab)) {
    tab[[1]] <- key_val_recode(
      x = tab[[1]], old = recode_tab[[1]], new = recode_tab[[2]]
    )
    tab[[2]] <- key_val_recode(
      x = tab[[2]], old = recode_tab[[1]], new = recode_tab[[2]]
    )
  }
  # group by p-values
  if (!is.null(p_breaks)) {
    tab[["sig_group"]] <- cut_pvals(tab[[7]], p_breaks)
  }
  tab
}

# ndf <- create_node_df(
#   n = 4, label = c("Y1", "Y2", "X1", "X2"), color = "black"
# )
# pdf <- create_edge_df(
#   from = c(4, 4, 3, 1),
#   to =   c(2, 1, 1, 2),
#   # penwidth = c(1, 1, 3, 5),
#   color = "black"
# )

#' Build a nodedf
#'
#' @param mod 
#' @param recode 
#'
#' @returns
#' @export
#'
#' @examples
#' mod = readRDS(
#'   file.path(
#'     "infection-modeling/data/model-objects",
#'     "selected-burden-SEM_maximal-cases.rds"
#'   )
#' )
#' (ndf = make_node_tab(
#'   mod, color = "black", shape = "rectangle", 
#'   fontsize = 12, width = 1
#' ))
make_node_tab <- function(mod, recode = recode_tab, ...){
  tab <- format_psem_coefs(mod)
  vars = unique(c(tab[[1]], tab[[2]]))
  if (!is.null(recode)) {
    vars = key_val_recode(vars, recode_tab[[1]], recode_tab[[2]])
  }
  
  ndf <- create_node_df(n = length(vars), label = vars, ...)
  ndf
  # tibble(id = seq_len(length(vars)), label = vars)
}

#' Title
#'
#' @param mod 
#' @param ... 
#'
#' @returns
#' @export
#'
#' @examples
#' 
make_path_tab <- function(mod, ...){
  tab <- format_psem_coefs(mod)
  vars = unique(c(tab[[1]], tab[[2]]))
  from_inx = match(tab[[2]], vars)
  to_inx = match(tab[[1]], vars)
  edge_list = rlang::list2(label = round(tab$effect, 2))
  edf <- create_edge_df(from = from_inx, to = to_inx, ...)
  edf
}
edf <- make_path_tab(mod)

#' Title
#'
#' @param x vector of effet values
#'
#' @returns
#' @export
#'
#' @examples
get_effect_color <- function(x){
  
}

#' Title
#'
#' @param mod 
#' @param node_attrs 
#' @param path_attrs 
#'
#' @returns
#' @export
#'
#' @examples
#' mod = readRDS(
#'   file.path(
#'     "infection-modeling/data/model-objects",
#'     "selected-burden-SEM_maximal-cases.rds"
#'   )
#' )
#' plot_psem(mod)
plot_psem <- function(
    mod, 
    node_attrs = list(
      shape = "rectangle", color = "black", width = 1, fontsize = 12
    ), 
    path_attrs = list(
      color = "black"
    )
  ) {
  coef_tab <- format_psem_coefs(mod)
  
  # TODO
  # update node_attrs to include hard-coded values
  node_attrs <- node_attrs %>% 
    append(
      list(
        # height = 0.5
      )
    )
  
  # TODO
  # update path_attrs to include hard-coded values
  path_attrs <- path_attrs %>% 
    append(
      list(
        # penwidth = 2
      )
    )
  
  # build node and edge tables
  nargs <- append(list(mod = mod), node_attrs)
  ndf <- do.call(make_node_tab, args = nargs)
  eargs <- append(list(mod = mod), path_attrs)
  edf <- do.call(make_path_tab, args = eargs)
  
  # build the figure
  create_graph(attr_theme = "bt") %>% 
    add_global_graph_attrs("layout", "dot", "graph") %>% 
    add_node_df(ndf) %>% 
    add_edge_df(edf) %>% 
    render_graph()
}

## ---- Example ----
pbreaks = c(0, 0.05, 0.1, 1)#, 
pcols = c("black", "grey70", "grey70")#, 
linetypes = c("solid", "solid", "dotted")#, 
penwidth_range = c(1, 8)

# coefficients
coef_tab <- mod$coefficients %>% 
  data.frame() %>% 
  rename(sig.star = "Var.9") %>% 
  mutate(
    pval_group = cut(
      P.Value, breaks = pbreaks, include.lowest = TRUE,
      labels(seq_len(length(pbreaks) - 1))
    ),
    pval_col = pcols[pval_group],
    pval_linetype = linetypes[pval_group],
    pval_penwidth = scales::rescale(Std.Estimate, to = penwidth_range)
  )

# all variables
the_vars <- unique(c(coef_tab$Response, coef_tab$Predictor))

path_nodes <- data.frame(var = the_vars, id = seq_len(length(the_vars)))

create_graph(attr_theme = "bt") %>% 
  add_global_graph_attrs("layout", "dot", "graph") %>%
  # add_global_graph_attrs("rankdir", "LR", "graph") %>% 
  add_nodes_from_table(path_nodes, label_col = var) %>% 
  add_edges_from_table(
    coef_tab, from_col = "Predictor", to_col = "Response", 
    from_to_map = "label"
  ) %>% 
  set_node_attrs(width, 1.5) %>% 
  set_node_attrs(fontsize, 20) %>% 
  set_edge_attrs(color, coef_tab$pval_col) %>% 
  set_edge_attrs(style, coef_tab$pval_linetype) %>% 
  set_edge_attrs(penwidth, coef_tab$pval_penwidth) %>% 
  render_graph()

## ---- OLDER ----

#' Function to build path diagram from pSEM objects
#'
#' @param mod piecewise SEM model fit (summary) object
#' @param pbreaks vector of p-value breaks (from 0 - 1, inclusive), passed
#' to \code{cut()}.
#' @param pcols vector of path colors by p-value. Length should be one shorter
#' than \code{pbreaks}.
#' @param linetypes vector of path line types by p-value. Length should be one
#' shorter than \code{pbreaks}.
#' @param penwidth_range vector of path line line widths by p-value. 
#' Length should be one shorter than \code{pbreaks}.
#'
#' @returns
#'
#' @examples
#' 
# diagram_psem <- function(
    # mod 
    pbreaks = c(0, 0.05, 0.1, 1)#, 
    pcols = c("black", "grey70", "grey70")#, 
    linetypes = c("solid", "solid", "dotted")#, 
    penwidth_range = c(1, 5)
# ) {
  
  # coefficients
  coef_tab <- mod$coefficients %>% 
    data.frame() %>% 
    rename(sig.star = "Var.9") %>% 
    mutate(
      pval_group = cut(
        P.Value, breaks = pbreaks, include.lowest = TRUE,
        labels(seq_len(length(pbreaks) - 1))
      ),
      pval_col = pcols[pval_group],
      pval_linetype = linetypes[pval_group],
      pval_penwidth = scales::rescale(Std.Estimate, to = penwidth_range)
    )
  
  # all variables
  the_vars <- unique(c(coef_tab$Response, coef_tab$Predictor))
  
  path_nodes <- data.frame(var = the_vars, id = seq_len(length(the_vars)))
  
  create_graph(attr_theme = "bt") %>% 
    add_global_graph_attrs("layout", "dot", "graph") %>%
    # add_global_graph_attrs("rankdir", "LR", "graph") %>% 
    add_nodes_from_table(path_nodes, label_col = var) %>% 
    add_edges_from_table(
      coef_tab, from_col = "Predictor", to_col = "Response", 
      from_to_map = "label"
    ) %>% 
    set_node_attrs(width, 1.2) %>% 
    set_edge_attrs(color, coef_tab$pval_col) %>% 
    set_edge_attrs(style, coef_tab$pval_linetype) %>% 
    set_edge_attrs(penwidth, coef_tab$pval_penwidth) %>% 
    render_graph()
  
# }


# library(DiagrammeR)
# 
# # Filter the coeficients to only significant paths:
# path_dat <- smry_alt_infect_psem2$coefficients %>% data.frame() %>% 
#   # filter(P.Value <= 0.1) %>% 
#   select(Response, Predictor, Std.Estimate, P.Value) 
# 
# var_lookup <- 
#   tibble(
#     var = unique(c(path_dat$Response, path_dat$Predictor)),
#     short_name = c(
#       "infected", "larvae\nattached", "nymphs\nattached", "resistance", "tolerance", "capture\ntime", 
#       "weight", "weather", "sex", "mature", "climate", "sex:mature"
#     )
#   )
# 
# path_dat <- path_dat %>% 
#   mutate(
#     Response = factor(Response, levels = var_lookup$var, labels = var_lookup$short_name),
#     Predictor = factor(Predictor, levels = var_lookup$var, labels = var_lookup$short_name),
#     path_color = case_when(
#       P.Value <= 0.05 ~ "black", 
#       P.Value <= 0.1 ~ "grey70",
#       .default = "grey70"
#     ),
#     path_linetype = if_else(P.Value <= 0.1, "solid", "dotted"),
#     path_penwidth = scales::rescale(Std.Estimate, to = c(1, 5)) %>% round(),
#     est_val = round(Std.Estimate, 2),
#     label = stringr::str_pad(est_val, 5)
#   )
# 
# # Get names of each node
# path_nodes <- tibble(
#   node_name = var_lookup$short_name,
#   id = seq_len(length(node_name))
# ) %>% 
#   mutate(
#     fillcolor = case_when(
#       node_name == "capture\ntime" ~ "cornflowerblue",
#       node_name == "resistance" ~ "forestgreen",
#       node_name == "tolerance" ~ "violet",
#       node_name == "infected" ~ "tomato",
#       node_name == "larvae\nattached" ~ "tan",
#       node_name == "nymphs\nattached" ~ "darkorange",
#       .default = "white"
#     ),
#   )

# gph <- create_graph(attr_theme = "bt") %>% 
#   add_global_graph_attrs("layout", "dot", "graph") %>% 
#   # add_global_graph_attrs("rankdir", "LR", "graph") %>% 
#   add_nodes_from_table(path_nodes, label_col = node_name) %>% 
#   add_edges_from_table(
#     path_dat, from_col = Predictor, to_col = Response, from_to_map = label
#   ) %>% 
#   # node attributes
#   set_node_attrs(width, 1.2) %>%
#   # set_node_attrs(fixedsize, FALSE) %>% 
#   set_node_attrs(color, "black") %>% 
#   set_node_attrs(fontcolor, "black") %>% 
#   set_node_attrs(fillcolor, path_nodes$fillcolor) %>% 
#   set_node_attrs(fontsize, 15) %>% 
#   # edge attributes
#   set_edge_attrs(len, 1) %>% 
#   set_edge_attrs(color, "black") %>% 
#   set_edge_attrs(color, path_dat$path_color) %>% 
#   set_edge_attrs(style, path_dat$path_linetype) %>% 
#   set_edge_attrs(penwidth, path_dat$path_penwidth) %>% 
#   set_edge_attrs(label, path_dat$label) %>% 
#   set_edge_attrs(fontcolor, path_dat$path_color) %>% 
#   set_edge_attrs(fontsize, 15) %>% 
#   set_edge_attrs(tooltip, path_dat$est_val)
# 
# {
#   set.seed(951)
#   render_graph(gph)
#   export_graph(gph, "infection-modeling/graphical-models/alt2_infect-PSEM.png")
#   }