# R script to simulate datasets under different parameters

# load packages
library(tidyverse) # data wrangling
library(simstudy) # data simulation
library(data.table) # data simulation
library(purrr) # loop for simulated data
library(openxlsx) # for saving data to Excel

# Set seed for reproducibility
set.seed(2024)

# Scenario B: Between-Subject Group Design --------------------------------

## Hierarchical Data -------------------------------------------------------

# Parameters for the simulations
sample_sizes <- c(74, 148)
between_subject_variances <- c(0.25, 0.75)

# Storage for all datasets
all_datasets <- list()

# Iterate over sample sizes and between-subject variances
for (sample_size in sample_sizes) {
  for (bs_variance in between_subject_variances) {
    
    # Define between-subject variability with Group assignment
    def1 <- defData(
      varname = "Group",
      formula = 0.50,
      dist = "binary",
      id = "id"
    )
    
    participant_variation <- defDataAdd(
      varname = "participant_variation",
      formula = "0",
      variance = bs_variance, 
      dist = "normal"
    )
    
    # Generate baseline data with Group and participant variation
    dd <- genData(sample_size, def1)
    dd <- addColumns(participant_variation, dd)
    
    # Generate clusters with repeated measurements for each participant
    dd <- genCluster(dd, cLevelVar = "id", numIndsVar = 3, level1ID = "total_trials")
    
    # Define the relationship of 'rating_raw' with 'Group' and 'participant_variation'
    defA <- defDataAdd(
      varname = "rating_raw",
      formula = "0.41*Group + participant_variation",
      dist = "nonrandom"
    )
    
    # Add 'rating_raw' based on 'Group' and 'participant_variation'
    dd <- addColumns(defA, dd)
    
    # Define ordinal categories
    rating_distribution <- c(0.40, 0.25, 0.15, 0.20)
    
    # Generate ordinal outcome 'rating' based on 'rating_raw'
    dd <- genOrdCat(dd, adjVar = "rating_raw", baseprobs = rating_distribution, 
                    catVar = "rating", idname = "total_trials")
    
    # Add columns for sample size, between-subject variance, and within-subject variance
    dd <- dd |>
      dplyr::select(c(id, Group, rating)) |> 
      group_by(id) %>%
      mutate(trial_number = row_number(),
             sample_size = sample_size,
             bs_variance = bs_variance,
             Group = case_when(Group == 1 ~ "PD",
                               Group == 0 ~ "Healthy")
      )
    
    # Store the dataset with a descriptive name
    dataset_name <- paste("sample_size", sample_size, "bs_variance", bs_variance, sep = "_")
    all_datasets[[dataset_name]] <- dd
  }
}

### Export to Excel ---------------------------------------------------------

# Create a grid of all combinations of the parameters
param_grid <- expand.grid(
  sample_size = sample_sizes,
  bs_variance = between_subject_variances
)

save_datasets_to_csv <- function(datasets, param_grid) {
  # Loop through datasets
  for (i in 1:length(datasets)) {
    
    # Get dataset parameters
    sample_size <- param_grid$sample_size[i]
    bs_variance <- param_grid$bs_variance[i]
    
    # Create the dataset name for the file
    dataset_name <- paste0("dataset_n", sample_size, "_bs", bs_variance)
    
    # Save the hierarchical dataset
    write.csv(datasets[[i]], here::here("Simulation", "Scenario_B", "Hierarchical", paste0(dataset_name, "_hierarchical.csv")), row.names = FALSE)
  }
}

# Call the function to save the datasets to CSV files
save_datasets_to_csv(all_datasets, param_grid)

## Non-Hierarchical Dataset ----------------------------------------------------

# # Filter function to apply
# filter_data <- function(df) {
#   df |>
#     filter(bs_variance == 0.5)
# }
# 
# # Apply the filter to each dataset
# filtered_datasets <- lapply(all_datasets, function(df) {
#   if (exists("bs_variance", where = df)) {
#     filter_data(df)
#   } else {
#     NULL # Return NULL if the dataset does not have the expected columns
#   }
# })

# Apply the transformation to each dataset in the list
summarized_datasets <- lapply(all_datasets, function(dataset) {
  dataset |> 
    as.data.frame() |> 
    mutate(rating_numeric = as.numeric(rating)) |> 
    group_by(id, Group) |> 
    summarise(max_rating = max(rating_numeric))
})

# Remove empty datasets
summarized_datasets <- keep(summarized_datasets, ~ nrow(.x) > 0)

### Export to Excel ---------------------------------------------------------

save_summarized_datasets_to_csv <- function(summarized_datasets, sample_sizes) {
  # Loop through summarized datasets
  for (i in 1:length(summarized_datasets)) {
    # Get the sample size for the dataset
    sample_size <- sample_sizes[i]
    
    # Create the dataset name for the file
    dataset_name <- paste0("summarized_dataset_n", sample_size, "_bs", bs_variance)
    
    # Define the output file path
    output_file <- here::here("Simulation", "Scenario_B", "Non_Hierarchical", paste0(dataset_name, "_summarized.csv"))
    
    # Save the summarized dataset to the specified CSV file location
    write.csv(summarized_datasets[[i]], output_file, row.names = FALSE)
  }
}

# Call the function to save the summarized datasets to CSV files
save_summarized_datasets_to_csv(summarized_datasets, sample_sizes)

