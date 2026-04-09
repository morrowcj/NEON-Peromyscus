library(tidyverse)

Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds") %>%
  mutate(
    species = updated_taxa
  ) %>% filter(!is.na(species))

out_dir = "infection-modeling/graphics/extra_figures"

if (!dir.exists(out_dir)){
  dir.create(out_dir)
}

Peros %>%
  select(iid, cap_prop_night, cap_num) %>% distinct() %>%
  ggplot(aes(x = cap_prop_night, fill = cap_num == 1)) +
  geom_histogram(color = "black") +
  labs(
    fill = "first capture?",
    title = "Capture time distribution"
  ) +
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.99, 0.99),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  ) +
  scale_fill_manual(values = c("cornflowerblue", "orange"))

ggsave(
  file.path(out_dir, "captime-distro.jpg"),
  width = 5, height = 0.8*5,
  dpi = 300
)

Peros %>%
  select(iid, capprop_shift, cap_num) %>% distinct() %>%
  filter(cap_num > 1) %>%
  ggplot(aes(x = capprop_shift)) +
  geom_histogram(color = "black", fill = "grey80") +
  labs(
    title = "Shift in capture time (recaptures only)"
  ) +
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.99, 0.99),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

ggsave(
  file.path(out_dir, "capshift-distro.jpg"),
  width = 5, height = 0.8*5,
  dpi = 300
)

Peros %>%
  select(iid, capprop_shift, cap_num, Bb_infected) %>%
  filter(cap_num > 1) %>%
  ggplot(aes(capprop_shift, fill = factor(Bb_infected))) +
  facet_wrap(
    ~Bb_infected, scales = "free_y", ncol = 1, labeller = label_both
  ) +
  geom_histogram(color = "black", bins = 10, show.legend = FALSE) +
  labs(title = "Shift distro.\n(recaptures only)") +
  theme_bw()

ggsave(
  file.path(out_dir, "capshift-distro_grouped.jpg"),
  width = 3, height = 0.8*3*3,
  dpi = 300
)

Peros %>%
  filter(!is.na(Bb_infected) & !is.na(capprop_shift) & cap_num > 1) %>%
  nrow()

Peros %>%
  select(iid, capprop_shift, cap_num) %>% distinct() %>%
  # filter(cap_num > 1) %>%
  ggplot(aes(x = capprop_shift, fill = factor(cap_num > 1))) +
  geom_histogram(color = "black") +
  labs(
    fill = "first capture?",
    title = "Shift in capture time (all observations)"
  ) +
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.99, 0.99),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  ) +
  scale_fill_manual(values = c("cornflowerblue", "orange"))

ggsave(
  file.path(out_dir, "capshift-distro_all-obs.jpg"),
  width = 5, height = 0.8*5,
  dpi = 300
)
