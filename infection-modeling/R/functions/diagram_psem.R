# function to plot a piecewise SEM model

library(tidyverse)
library(DiagrammeR)

mod = sel_smry

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