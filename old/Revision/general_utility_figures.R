# General utility figures
library(cowplot)
library(tidyverse)

# Function to add title to plots
draw_label_theme <- function(label, theme = NULL, element = "text", ...) {
  if (is.null(theme)) {
    theme <- ggplot2::theme_get()
  }
  if (!element %in% names(theme)) {
    stop("Element must be a valid ggplot theme element name")
  }
  
  elements <- ggplot2::calc_element(element, theme)
  
  cowplot::draw_label(label, 
                      fontfamily = elements$family,
                      fontface = elements$face,
                      colour = elements$color,
                      size = elements$size,
                      ...
  )
}

# Swallowing --------------------------------------------------------------

# Extract the data frames
swallowing_data_original <- swallowing$data_original |> 
  mutate(dataset_type = "Original")

swallowing_data_synthetic_first <- swallowing_data_synthetic$syn[[1]]

# Combine the data frames
swallowing_combined_data <- swallowing_data_synthetic_first |> 
  mutate(dataset_type = "Synthetic") |> 
  rbind(swallowing_data_original)

# Create a new categorical variable
swallowing_combined_data <- swallowing_combined_data %>%
  mutate(severity_category = ifelse(laryngeal_vestibule_severity_rating == 0, "0%", "> 0"))

# Bar plot for frequency of 0's
p1 <- swallowing_combined_data |> 
  filter(severity_category == "0%") |> 
  ggplot(aes(x = severity_category, fill = dataset_type)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  labs(x = "",
       y = "Frequency") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        # axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 8))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 470))

# Density plot for values > 0
p2 <- swallowing_combined_data |> 
  filter(severity_category == "> 0") |> 
  ggplot(aes(x = laryngeal_vestibule_severity_rating, color = dataset_type, fill = dataset_type)) +
  geom_density(alpha = 0.80) +
  scale_fill_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  scale_color_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  # scale_x_continuous(limits = c(1, 50)) +
  labs(x = "",
       y = "Density",
       color = "Dataset Type",
       fill = "Dataset Type") +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.01, 0.45),
                     breaks = c(0.01, 0.1, 0.2, 0.3, 0.4)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.justification = "center",
        axis.title.y = element_text(margin = margin(r = 8)))

# Extract the legend from p2
legend_components <-  cowplot::get_plot_component(p2, 'guide-box-top', return_all = TRUE)
legend <- cowplot::ggdraw(legend_components)

# Add title
title <- ggdraw() +
  draw_label_theme("A: Swallowing (Curtis et al., 2023)", 
                   theme = theme_cowplot(), element = "plot.title",
                   x = 0.075, hjust = 0, vjust = 1)

# Combine plots without legends
combined_plots <- cowplot::plot_grid(p1, p2 + theme(legend.position = "none"), rel_widths = c(0.40, 1))

# Arrange the plots and legend
final_plot <- cowplot::plot_grid(
  title,
  combined_plots,
  ncol = 1,
  rel_heights = c(
    0.2, 
    3)
)

# Add a shared x-axis label
swallowing_general_utility_plot <- cowplot::ggdraw(final_plot) +
  cowplot::draw_label("Laryngeal Vestibule Residue Rating", x = 0.5, y = 0.02, 
                      vjust = 0, hjust = 0.5, size = 15)

# Articulation ------------------------------------------------------------

# Extract the data frames
articulation_data_original <- articulation$data_original |> 
  mutate(dataset_type = "Original")

articulation_data_synthetic_first <- articulation_data_synthetic$syn[[1]]

# Combine the data frames
articulation_combined_data <- articulation_data_synthetic_first |> 
  mutate(dataset_type = "Synthetic") |> 
  rbind(articulation_data_original)

# Scatterplot
articulation_p1 <- 
  articulation_combined_data |> 
  ggplot(aes(x = VSA_b, y = Int, color = dataset_type, fill = dataset_type)) +
  geom_point(alpha = 0.80, size = 2.5) +
  scale_fill_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  scale_color_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  scale_x_continuous(breaks = c(1, 3, 6, 9, 12)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
  labs(x = "Vowel Space Area",
       y = "Intelligibility",
       color = "Dataset Type",
       fill = "Dataset Type") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(r = 8)),
        axis.title.y = element_text(margin = margin(t = 8)))

# Add title
articulation_title <- ggdraw() +
  draw_label_theme("B: Articulation (Thompson et al., 2023)", 
                   theme = theme_cowplot(), element = "plot.title",
                   x = 0.075, hjust = 0, vjust = 1)

# Combine plot with title
articulation_final_plot <- cowplot::plot_grid(
  articulation_title,
  articulation_p1,
  ncol = 1,
  rel_heights = c(0.2, 3) # Adjust the relative widths as needed
)

# Fluency -----------------------------------------------------------------

# Extract the data frames
fluency_data_original <- fluency$data_original |> 
  mutate(dataset_type = "Original")

fluency_data_synthetic_first <- fluency_data_synthetic$syn[[1]]

# Combine the data frames
fluency_combined_data <- fluency_data_synthetic_first |> 
  mutate(dataset_type = "Synthetic") |> 
  rbind(fluency_data_original)

# Scatterplot
fluency_p1 <- fluency_combined_data |> 
  mutate(Group = case_when(Group == "AWS" ~ "Adults Who Stutter",
                           Group == "NT" ~ "Neurotypical")) |> 
  ggplot(aes(x = Nonwordrepetition, color = dataset_type, fill = dataset_type)) +
  geom_density(alpha = 0.80) +
  scale_fill_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  scale_color_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  scale_x_continuous(limits = c(-1, 17)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Non-word Repetition",
       y = "Density",
       color = "Dataset Type",
       fill = "Dataset Type") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(r = 8)),
        axis.title.y = element_text(margin = margin(t = 8))) +
  facet_wrap(~Group, nrow = 1, scales = "free_x")

# Add title
fluency_title <- ggdraw() +
  draw_label_theme("C: Fluency (Elsherifa et al., 2021)", 
                   theme = theme_cowplot(), element = "plot.title",
                   x = 0.075, hjust = 0, vjust = 1)

# Combine plot with title
fluency_final_plot <- cowplot::plot_grid(
  fluency_title,
  fluency_p1,
  ncol = 1,
  rel_heights = c(0.2, 3) # Adjust the relative widths as needed
)

# Voice -------------------------------------------------------------------

# Extract the data frames
voice_data_original <- voice$data_original |> 
  mutate(dataset_type = "Original")

voice_data_synthetic_first <- voice_data_synthetic$syn[[1]]

# Combine the data frames
voice_combined_data <- voice_data_synthetic_first |> 
  mutate(dataset_type = "Synthetic") |> 
  rbind(voice_data_original)

# Scatterplot
voice_p1 <- voice_combined_data |> 
  mutate(median_of_raters = case_when(median_of_raters == 0 ~ "No",
                                      median_of_raters == 1 ~ "Mild",
                                      median_of_raters == 2 ~ "Moderate"),
         median_of_raters = factor(median_of_raters,
                                   levels = c("No", "Mild", "Moderate"))) |> 
  ggplot(aes(x = efn_sd_d_b, color = dataset_type, fill = dataset_type)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  scale_color_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  scale_x_continuous(limits = c(-1, 17)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Variability of Nasality",
       y = "Density",
       color = "Dataset Type",
       fill = "Dataset Type") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(r = 8)),
        axis.title.y = element_text(margin = margin(t = 8))) +
  facet_wrap(~median_of_raters, nrow = 1, scales = "free_x")

# Add title
voice_title <- ggdraw() +
  draw_label_theme("D: Voice (Novotny et al., 2016)", 
                   theme = theme_cowplot(), element = "plot.title",
                   x = 0.075, hjust = 0, vjust = 1)

# Combine plot with title
voice_final_plot <- cowplot::plot_grid(
  voice_title,
  voice_p1,
  ncol = 1,
  rel_heights = c(0.2, 3) # Adjust the relative widths as needed
)

# Hearing -----------------------------------------------------------------

# Extract the data frames
hearing_data_original <- hearing$data_original |> 
  mutate(dataset_type = "Original")

hearing_data_synthetic_first <- hearing_data_synthetic$syn[[1]]

# Combine the data frames
hearing_combined_data <- hearing_data_synthetic_first |> 
  mutate(dataset_type = "Synthetic") |> 
  rbind(hearing_data_original)

# Scatterplot
hearing_p1 <- 
  hearing_combined_data |> 
  ggplot(aes(x = thre, y = subj, color = dataset_type, fill = dataset_type)) +
  geom_point(alpha = 0.80, size = 2.5) +
  scale_fill_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  scale_color_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  labs(x = "Auditory Localization",
       y = "Subject ID",
       color = "Dataset Type",
       fill = "Dataset Type") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(r = 8)),
        axis.title.y = element_text(margin = margin(t = 8)))

hearing_p2 <- 
  hearing_combined_data |> 
  mutate(group = case_when(group == "CB" ~ "Congentially Blind",
                           group == "SC" ~ "Sighted Controls")) |> 
  ggplot(aes(x = thre, color = dataset_type, fill = dataset_type)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  scale_color_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  labs(x = "Auditory Localization",
       y = "Density",
       color = "Dataset Type",
       fill = "Dataset Type") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(r = 8)),
        axis.title.y = element_text(margin = margin(t = 8))) +
  facet_wrap(~group) +
  scale_y_continuous(expand = c(0, 0))

# # Combine plots without legends
# hearing_combined_plots <- cowplot::plot_grid(hearing_p1, 
#                                              hearing_p2, rel_widths = c(1, 0.7))

# Add title
hearing_title <- ggdraw() +
  draw_label_theme("E: Hearing (Battal et al., 2019)", 
                   theme = theme_cowplot(), element = "plot.title",
                   x = 0.075, hjust = 0, vjust = 1)

# Combine plot with title
hearing_final_plot <- cowplot::plot_grid(
  hearing_title,
  hearing_p1,
  ncol = 1,
  rel_heights = c(0.2, 3) # Adjust the relative widths as needed
)

hearing_final_plot2 <- cowplot::plot_grid(
  hearing_title,
  hearing_p2,
  ncol = 1,
  rel_heights = c(0.2, 3) # Adjust the relative widths as needed
)

# Communication Modalities ---------------------------------------------------------------------

# Extract the data frames
aac_data_original <- aac$data_original |> 
  mutate(dataset_type = "Original")

aac_data_synthetic_first <- aac_data_synthetic$syn[[1]]

# Combine the data frames
aac_combined_data <- aac_data_synthetic_first |> 
  mutate(dataset_type = "Synthetic") |> 
  rbind(aac_data_original)

# Scatterplot
aac_p1 <- 
  aac_combined_data |> 
  ggplot(aes(x = value, color = dataset_type, fill = dataset_type)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  scale_color_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  labs(x = "Lack of/limited internet",
       y = "Frequency",
       color = "Dataset Type",
       fill = "Dataset Type") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(r = 8)),
        axis.title.y = element_text(margin = margin(t = 8)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  facet_wrap(~var, nrow = 1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 245))

# Add title
aac_title <- ggdraw() +
  draw_label_theme("F: Communication Modalities (King et al., 2022)", 
                   theme = theme_cowplot(), element = "plot.title",
                   x = 0.075, hjust = 0, vjust = 1)

# Combine plot with title
aac_final_plot <- cowplot::plot_grid(
  aac_title,
  aac_p1,
  ncol = 1,
  rel_heights = c(0.2, 3) # Adjust the relative widths as needed
)

## Put plots together ------------------------------------------------------

# Add plot to the first plot above
plot_a <- cowplot::plot_grid(
  swallowing_general_utility_plot,
  fluency_final_plot,
  hearing_final_plot2,
  ncol = 1
)

# Add plots together
plot_b <- cowplot::plot_grid(
  articulation_final_plot,
  voice_final_plot,
  aac_final_plot,
  ncol = 1
)

# Add all plots together
combined_plots <- cowplot::plot_grid(
  plot_a,
  plot_b,
  ncol = 2
)

# Add legend
combined_plots2 <- cowplot::plot_grid(
  legend,
  combined_plots,
  ncol = 1,
  rel_heights = c(0.025, 1)
)

ggsave(filename = "combined_plots.png", plot = combined_plots2,
       device = "png", dpi = 300, bg = "white", height = 15, width = 13)

# Language - Kearney ------------------------------------------------------

# Extract the data frames
language_data_original <- language$data_original |> 
  mutate(dataset_type = "Original")

language_data_synthetic_first <- language_data_synthetic$syn[[1]]

# Combine the data frames
language_combined_data <- language_data_synthetic_first |> 
  mutate(dataset_type = "Synthetic") |> 
  rbind(language_data_original)

# Scatterplot
language_p1 <- 
  language_combined_data |> 
  ggplot(aes(x = edu, y = tpost_cat_read_total, color = dataset_type, fill = dataset_type)) +
  geom_point(alpha = 0.80, size = 2.5) +
  scale_fill_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  scale_color_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  labs(x = "Education",
       y = "Reading Score",
       color = "Dataset Type",
       fill = "Dataset Type") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(r = 8)),
        axis.title.y = element_text(margin = margin(t = 8)))

# Add title
language_title <- ggdraw() +
  draw_label_theme("A: Language (Kearney et al., 2023)", 
                   theme = theme_cowplot(), element = "plot.title",
                   x = 0.075, hjust = 0, vjust = 1)

# Combine plot with title
language_final_plot <- cowplot::plot_grid(
  language_title,
  language_p1,
  ncol = 1,
  rel_heights = c(0.2, 3) # Adjust the relative widths as needed
)

# Language - Single Subject -----------------------------------------------



# Cognition ---------------------------------------------------------------

# Extract the data frames
cognition_data_original <- cognition_data_original |> 
  mutate(dataset_type = "Original")

cognition_data_synthetic_first <- cognition_data_synthetic$syn[[1]]

# Combine the data frames
cognition_combined_data <- cognition_data_synthetic_first |> 
  mutate(dataset_type = "Synthetic") |> 
  rbind(cognition_data_original)

# Data viz
cognition_p1 <- 
  cognition_combined_data |> 
  mutate(Correct = as.numeric(as.character(Correct)),
         Group_NC = case_when(Group_NC == "BasicFace" ~ "Basic Face",
                              Group_NC == "BasicEmoji" ~ "Basic Emoji",
                              Group_NC == "SocialEmoji" ~ "Social Emoji")) |> 
  group_by(Group_NC, Condition, dataset_type) %>%
  summarize(Proportion_Correct = mean(Correct),
            Count = n(),
            .groups = 'drop') |> 
  ggplot(aes(x = dataset_type, y = Proportion_Correct, fill = as.factor(dataset_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Condition) +
  scale_fill_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  labs(x = "",
       y = "Proportion Correct",
       fill = "Group NC") +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1),
                     labels = scales::percent)

# Add title
cognition_title <- ggdraw() +
  draw_label_theme("C: Cognition (Clough  et al., 2023)", 
                   theme = theme_cowplot(), element = "plot.title",
                   x = 0.075, hjust = 0, vjust = 1)

# Combine plot with title
cognition_final_plot <- cowplot::plot_grid(
  cognition_title,
  cognition_p1,
  ncol = 1,
  rel_heights = c(0.2, 3) # Adjust the relative widths as needed
)

# Social Communication ----------------------------------------------------

# Extract the data frames
social_data_original <- social_data_original |> 
  mutate(dataset_type = "Original")

social_data_synthetic_first <- social_data_synthetic$syn[[1]]

# Combine the data frames
social_combined_data <- social_data_synthetic_first |> 
  mutate(dataset_type = "Synthetic") |> 
  rbind(social_data_original)

# Data viz
social_p1 <- 
  social_combined_data |> 
  mutate(ParGroup = case_when(ParGroup == "ASD" ~ "Autism Spectrum Disorder",
                              ParGroup == "TD" ~ "Neurotypical")) |> 
  ggplot(aes(x = NVIQ, fill = as.factor(dataset_type))) +
  geom_density(alpha = 0.80) +
  facet_wrap(~ ParGroup) +
  scale_fill_manual(values = c("Synthetic" = "#184765", "Original" = "#5EC0D0")) +
  labs(x = "Non-Verbal IQ",
       y = "Density",
       fill = "Group NC") +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.04))

# Add title
social_title <- ggdraw() +
  draw_label_theme("D: Social (Chanchaochai & Schwarz, 2023)", 
                   theme = theme_cowplot(), element = "plot.title",
                   x = 0.075, hjust = 0, vjust = 1)

# Combine plot with title
social_final_plot <- cowplot::plot_grid(
  social_title,
  social_p1,
  ncol = 1,
  rel_heights = c(0.2, 3) # Adjust the relative widths as needed
)

## Put plots together ------------------------------------------------------

# Add plot to the first plot above
plot_a1 <- cowplot::plot_grid(
  language_final_plot,
  cognition_final_plot,
  ncol = 1
)

# Add plots together
plot_b1 <- cowplot::plot_grid(
  NULL,
  social_final_plot,
  ncol = 1
)

# Add all plots together
combined_plots1 <- cowplot::plot_grid(
  plot_a1,
  plot_b1,
  ncol = 2
)

# Add legend
combined_plots_2 <- cowplot::plot_grid(
  NULL,
  legend,
  combined_plots1,
  ncol = 1,
  rel_heights = c(0.01, 0.025, 1)
)

ggsave(filename = "combined_plots2.png", plot = combined_plots_2,
       device = "png", dpi = 300, bg = "white", height = 9, width = 11)

