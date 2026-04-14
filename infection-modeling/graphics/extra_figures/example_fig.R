library(tidyverse)

df = tibble(
  group = factor(rep(1:3, each = 2)),
  group_mean = rep(c(-0.3, 0.1, 0.5), each = 2),
  treatment = factor(rep(1:2, times = 3)),
  treatment_mean = rep(c(0, 0.2), times = 3),
  sig = c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE),
  sig_mean = if_else(sig, sig_mean, 0),
  mean = group_mean + treatment_mean + sig_mean,
  var = if_else(sig, abs(mean - 0) * 0.8, abs(mean) * 1.2),
  lower = mean - var,
  upper = mean + var
)

group_shapes = c(21, 23, 24)
group_colors = c("blue", "orange", "purple")

df %>%
  ggplot(aes(
    x = treatment, y = mean, ymin = lower, ymax = upper,
    shape = group, col = group,
    linetype = sig,
    fill = interaction(group, sig)
  )) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
  geom_pointrange(position = position_dodge(0.5), aes(group = group)) +
  scale_shape_manual(values = group_shapes) +
  scale_color_manual(values = group_colors) +
  scale_fill_manual(values = c(rep("white", 3), group_colors)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  guides(
    colour = guide_legend(
      override.aes = list(shape = group_shapes, fill = "white")),
    linetype = guide_legend(
      override.aes = list(shape = group_shapes[1], fill = c("white", "black"))
    ),
    fill = FALSE
  ) +
  theme_bw()
