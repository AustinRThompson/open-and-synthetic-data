# load packages
library(tidyverse)

# Scenario A --------------------------------------------------------------

source(here::here("Simulation", "sim_design_scenario_a.R"))

df_a <- datasets[[1]]

# visualize example dataset
scenario_a_viz <- df_a |> 
  mutate(time = case_when(time == 0 ~ "Pre", 
                          time == 1 ~ "Post"),
         time = factor(time,
                       levels = c("Pre", "Post"))) |> 
  ggplot(aes(x = time, y = peak_flow)) +
  geom_point() +
  cowplot::theme_cowplot() +
  labs(x = "",
       y = "Cough Strength (L/s)", 
       title = "Scenario A") +
  scale_y_continuous(limits = c(1, 5)) +
  coord_flip()

# Scenario B --------------------------------------------------------------

source(here::here("Simulation", "sim_design_scenario_b.R"))

df_b <- summarized_datasets[[1]]

# visualize example dataset
scenario_b_viz <- df_b |> 
  group_by(Group, rating) |>
  tally() |> 
  ungroup() |> 
  add_row(Group = "PD", rating = 1, n = 0) |>
  group_by(Group) |>
  mutate(percentage = n / sum(n) * 100) |>
  ungroup() |> 
  mutate(rating = case_when(rating == 1 ~ "None",
                            rating == 2 ~ "Mild",
                            rating == 3 ~ "Moderate", 
                            rating == 4 ~ "Severe"),
         rating = factor(rating,
                         levels = c("None",
                                    "Mild",
                                    "Moderate",
                                    "Severe"))) |> 
  ggplot(aes(x = factor(rating), y = n, fill = Group)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Scenario B",
    x = "Rating",
    y = "Frequency",
    fill = "Group"
  ) +
  cowplot::theme_cowplot() +
  theme(legend.position = "top",
        legend.title = element_blank())

# Scenario C --------------------------------------------------------------

source(here::here("Simulation", "sim_design_scenario_c.R"))

df_c <- summary_data

# visualize example dataset
scenario_c_viz <- df_c |> 
  ggplot(aes(x = session, y = proportion_positive, group = item_type, color = item_type)) +
  geom_vline(xintercept = 5, linetype = "dashed", alpha = 0.33) +
  geom_line() +
  geom_point() +
  labs(
    title = "Scenario C",
    x = "Session",
    y = "Proportion of Correct Responses",
    color = "Condition"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  cowplot::theme_cowplot() +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  scale_x_continuous(limits = c(1, 15),
                     breaks = c(seq(1:15)))

# Combine plots -----------------------------------------------------------
combined_figures <- cowplot::plot_grid(scenario_a_viz, scenario_b_viz, scenario_c_viz, 
                                       nrow = 3, align = "hv")

ggsave(filename = here::here("Simulation", "example_data_figure.png"), plot = combined_figures, device = png, dpi = 300, bg = "white",
       height = 12, width = 8)
  