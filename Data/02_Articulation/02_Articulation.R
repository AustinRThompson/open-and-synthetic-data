# This script recreates the analysis in Thompson et al. (2023)

# Packages ----
library(tidyverse)

# Loading in the Data  ----
data <- rio::import(
  file = "data_Acoustic Measures.csv"
) |>
  
  # Remove the reliability trials that have "_rel" in the SpeakerID variable
  dplyr::filter(
    !grepl(
      pattern = "_rel",
      x = SpeakerID
  ))


# Analysis  ----
# Here, we recreate the relationship between VSA and intelligibility,
# t(38) = 3.64, p < .001, 95% CI [2.60, 9.14].

model <- lm(Int_OT ~ VSA_b,
            data = data)
summary(model)

# Pasting it together to get the APA write-up
apa <- paste0(
  "t(",
  
  # DF
  df <- model[["df.residual"]],
  ") = ",
  
  # t statistic
  t <- summary(model)$coefficients |>
    as.data.frame() |>
    dplyr::filter(row.names(summary(model)$coefficients) == 'VSA_b') |>
    dplyr::select(`t value`) |>
    round(digits = 2),
  
  ", p < .001, 95% CI [",
  
  CI_lower <- confint(model, level = 0.95) |>
    as.data.frame() |>
    dplyr::filter(row_number() == 2) |> # Get the 2nd row containing VSA_b
    dplyr::select(`2.5 %`) |>
    round(digits = 2),
  
  ", ",
  
  CI_upper <- confint(model, level = 0.95) |>
    as.data.frame() |>
    dplyr::filter(row_number() == 2) |> # Get the 2nd row containing VSA_b
    dplyr::select(`97.5 %`) |>
    round(digits = 2),
  
  "]"
  )
apa # Here, we recreate the same APA write-up reported in the study

