library(dplyr)
library(ggnetwork)
library(ggarrow)

set.seed(4)

t <- seq(0, 2 * pi, length.out = 11) ## 11 points around a radius
l <- rep(c(1, 0.4), length.out = 11) ## alternating distance from origin

node_df <- tibble(
  x = cos(t) * l,
  y = sin(t) * l,
  cent_dist = l
) %>%
  mutate(
    id = row_number()
  )

degrees_to_frac = function(d){
  d / 360
}

frac_to_pi <- function(f){
  f * (2 * pi)
}

slope_to_degrees = function(b){
  atan(b)
}

## sample node pairs to make edges
edge_df <- replicate(10, sort(sample(node_df$id, 2))) %>%
  t() %>%
  data.frame() %>%
  setNames(c("from", "to")) %>%
  tibble() %>%
  filter(from!=to) %>% distinct() %>%
  arrange(desc(to), desc(from))

## get the segment engds for both points
path_df <- edge_df %>%
  left_join(
    node_df %>% select(from = id, from_x = x, from_y = y), by = "from",
  ) %>%
  left_join(
    node_df %>% select(to = id, to_x = x, to_y = y), by = "to"
  )

# visualize
ggplot(data = node_df, aes(x = x, y = y)) +
  # geom_point() +
  geom_label(aes(label = id)) +
  geom_arrow_segment(
    data = path_df,
    aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
    resect = 5
  )



## ---- cleaner

nds <- tibble(
  id = c(1:5),
  name = c("z1", "z2", "x1", "x2", "y"),
  group = c("z", "z", "x", "x", "y"),
  x = c(-0.5, 0.5, -1, 1, 0),
  y = c(-1, -1, 0, 0, 1)
)

eds <- tibble(
  from = c("x1", "x2", "x2", "z1", "z2", "z2"),
  to = c("y", "y", "x1", "x2", "y", "x2"),
  effect = c(1, 3/2, 1/4, 1/3, 1/2, 1/2)
) %>%
  mutate(linewidth = scales::rescale(effect, to = c(1, 3)))

eds <- eds %>%
  left_join(
    nds %>%
      select(
        from = name, from_id = id, from_x = x, from_y = y, from_group = group
      )
  ) %>%
  left_join(
    nds %>% select(to = name, to_id = id, to_x = x, to_y = y)
  ) %>%
  mutate(label_x = (from_x + to_x) / 2, label_y = (from_y + to_y) / 2)

ggplot(nds, aes(x = x, y = y)) +
  geom_point(
    col = "black", size = 30, show.legend = FALSE, shape = 21, fill = "white"
  ) +
  geom_text(aes(label = name), size = 11) +
  geom_arrow_segment(
    data = eds,
    aes(
      x = from_x, y = from_y, xend = to_x, yend = to_y, col = from_group,
    ), linewidth = eds$linewidth,
    resect = 12
  ) +
  # geom_arrow_segment(
  #   data = eds,
  #   aes(x = from_x * 1.1, y = from_y * 1.1, xend = to_x * 1.1, yend = to_y * 1.1),
  #   col = "purple", resect = 12
  # ) +
  geom_label(
    data = eds, label.size = NA, show.legend = FALSE,
    aes(x = label_x, y = label_y, label = round(effect, 1), col = from_group)
  ) +
  scale_x_continuous(expand = c(.2, 0)) +
  scale_y_continuous(expand = c(.2, 0)) +
  theme_blank() +
  theme(
    legend.position = "inside", legend.position.inside = c(1, 1),
    legend.justification = c(1, 1)
  ) +
  guides(linewidth = FALSE)
