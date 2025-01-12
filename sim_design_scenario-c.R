# R script to simulate datasets under different parameters

# load packages
library(tidyverse) # data wrangling
library(simstudy) # data simulation
library(data.table) # data simulation
library(purrr) # loop for simulated data
library(openxlsx) # for saving data to Excel

# Set seed for reproducibility
set.seed(2024)

# Scenario C: Single-Subject Design ---------------------------------------

## Hierarchical Data -------------------------------------------------------

# Define the different parameter values
sample_sizes <- c(1)
between_subject_variances <- c(0.25, 0.75)
within_subject_variances <- c(0.25, 0.75)

# Create a grid of all combinations of the parameters
param_grid <- expand.grid(
  sample_size = sample_sizes,
  bs_variance = between_subject_variances,
  ws_variance = within_subject_variances
)

# Function to generate data based on parameters
generate_within_data <- function(sample_size, bs_variance, ws_variance) {
  
  # Specify between-subject variability
  defc <- defData(
    varname = "a",
    formula = 0,
    variance = bs_variance,
    id = "grp"
  )
  
  # Specify number of trials
  defc <- defData(defc,
                  varname = "size",
                  formula = 3,
                  dist = "nonrandom") # number of trials
  
  # Specify treatment effect and within-subject variability
  defA <- defDataAdd(
    varname = "peak_flow",
    formula = "2.50 + 0.50*period + a",
    dist = "normal",
    variance = ws_variance
  ) # within-subject variability
  
  # Generate simulated data
  dc <- genData(sample_size, defc) # number of participants
  dd <- genCluster(dc, cLevelVar = "grp", numIndsVar = "size", level1ID = "total_trials")
  dd <- addPeriods(dd, nPeriods = 2, idvars = "total_trials")
  dd <- addColumns(defA, dd)
  
  # Process and clean the data
  dd <- dd |>
    group_by(grp, period) |>
    mutate(trial_number = row_number()) |>
    rename(time = period, subject_id = grp) |>
    select(subject_id, time, peak_flow, timeID, trial_number) |>
    as.data.frame()
  
  # Add columns for sample size, between-subject variance, and within-subject variance
  dd <- dd |>
    mutate(
      sample_size = sample_size,
      bs_variance = bs_variance,
      ws_variance = ws_variance
    )
  
  return(dd)
}

# Generate datasets using the parameter grid
datasets <- param_grid |>
  pmap(function(sample_size, bs_variance, ws_variance) {
    generate_within_data(sample_size, bs_variance, ws_variance)
  })

# Name the datasets for clarity
names(datasets) <- paste0("dataset_n", param_grid$sample_size, 
                          "_bs", param_grid$bs_variance, 
                          "_ws", param_grid$ws_variance)

# Example: Access the first dataset
head(datasets$dataset_n25_bs0.25_ws0.25)

### Export to Excel ---------------------------------------------------------

save_datasets_to_csv <- function(datasets, param_grid) {
  # Loop through datasets
  for (i in 1:length(datasets)) {
    # Get dataset parameters
    sample_size <- param_grid$sample_size[i]
    bs_variance <- param_grid$bs_variance[i]
    ws_variance <- param_grid$ws_variance[i]
    
    # Create the dataset name for the file
    dataset_name <- paste0("dataset_n", sample_size, "_bs", bs_variance, "_ws", ws_variance)
    
    # Save the hierarchical dataset
    write.csv(datasets[[i]], here::here("Simulation", "Scenario_A", "Hierarchical", paste0(dataset_name, "_hierarchical.csv")), row.names = FALSE)
  }
}

# Call the function to save the datasets to CSV files
save_datasets_to_csv(datasets, param_grid)

## Non-Hierarchical Dataset ----------------------------------------------------

# # Filter function to apply
# filter_data <- function(df) {
#   df |>
#     filter(bs_variance == 0.25 & ws_variance == 0.25)
# }
# 
# # Apply the filter to each dataset
# filtered_datasets <- lapply(datasets, function(df) {
#   if (exists("bs_variance", where = df) & exists("ws_variance", where = df)) {
#     filter_data(df)
#   } else {
#     NULL # Return NULL if the dataset does not have the expected columns
#   }
# })

# Function to summarize each dataset
summarize_dataset <- function(data) {
  data |>
    group_by(subject_id, time, sample_size, bs_variance, ws_variance) |>
    summarise(mean_peak_flow = mean(peak_flow, na.rm = TRUE)) |>
    ungroup()
}

# Apply the summarization function to each dataset in the list
summarized_datasets <- datasets |>
  map(summarize_dataset)

# Remove empty datasets
summarized_datasets <- keep(summarized_datasets, ~ nrow(.x) > 0)

### Export to Excel ---------------------------------------------------------

save_summarized_datasets_to_csv <- function(summarized_datasets, sample_sizes) {
  # Loop through summarized datasets
  for (i in 1:length(summarized_datasets)) {
    # Get dataset parameters
    sample_size <- param_grid$sample_size[i]
    bs_variance <- param_grid$bs_variance[i]
    ws_variance <- param_grid$ws_variance[i]
    
    # Create the dataset name for the file
    dataset_name <- paste0("summarized_dataset_n", sample_size, "_bs", bs_variance, "_ws", ws_variance)
    
    # Define the output file path
    output_file <- here::here("Simulation", "Scenario_A", "Non_Hierarchical", paste0(dataset_name, "_summarized.csv"))
    
    # Save the summarized dataset to the specified CSV file location
    write.csv(summarized_datasets[[i]], output_file, row.names = FALSE)
  }
}

# Call the function to save the summarized datasets to CSV files
save_summarized_datasets_to_csv(summarized_datasets, sample_sizes)






