# This R script reproduces the code in the tutorial section of the results.

# load required packages
library(tidyverse) # data wrangling
library(synthpop) # R package to generate synthetic data

# Study 1: Study 1: Normative Reference Values for Swallowing Outc --------

# load original data
swallowing_original_data <-
  # read csv file from appropriate path
  read.csv(here::here("Data/01_Swallowing/norms_ratings.csv")) |>
  # clean variable names
  janitor::clean_names() |>
  # select only relevant variables from dataset
  dplyr::select(c(study_id, bolus_consistency, 
                  laryngeal_vestibule_severity_rating)) |> 
  mutate(
    # convert study_id and bolus_consistency to factors
    study_id = as.factor(study_id),
    bolus_consistency = as.factor(bolus_consistency),
    # express laryngeal_vestibule_severity_rating as a %
    laryngeal_vestibule_severity_rating = laryngeal_vestibule_severity_rating/100
  )

# Create a synthetic dataset
synthetic_data <-
  syn(swallowing_original_data, # name of the original data
      method = "ctree", # CART model to generate synthetic data
      m = 1 # number of synthetic datasets to generate
  )

# Comparison of original and synthetic datasets with synthpop package
swallowing_comparison <- compare(
  synthetic_data, # synthetic dataset
  swallowing_original_data, # original dataset
  vars = c("bolus_consistency",
           "laryngeal_vestibule_severity_rating"), # variables for comparison
  stat = "counts", # Present the raw counts for each variable
  cols = c("#62B6CB", "#1B4965") # Setting the colours in the plot
)

# Study 2: Study 2: Vowel Acoustics as Predictors of Speech Intell --------

# import original data
articulation_original_data <- rio::import(
  file = here::here("Data", "02_Articulation", "data_Acoustic Measures.csv")
) |>
  # Remove the reliability trials that have "_rel" in the SpeakerID variable
  dplyr::filter(
    !grepl(
      pattern = "_rel",
      x = SpeakerID
    )) |>
  # Selecting just the variables we need
  dplyr::select(
    SpeakerID, # ID
    VSA_b, # VSA in Bark
    Int = Int_OT # intelligibility (orthographic transcriptions)
  )

# generate synthetic dataset
articulation_synthetic_dataset <- syn(articulation_original_data,
                                      m = 1,
                                      seed = 2024)

# Comparison of original and synthetic datasets with synthpop package
articulation_comparison <- compare(
  articulation_synthetic_dataset, # synthetic dataset
  articulation_original_data, # original dataset
  vars = c("VSA_b",
           "Int"), # variables for comparison
  stat = "counts", # Present the raw counts for each variable
  cols = c("#62B6CB", "#1B4965") # Setting the colours in the plot
)
