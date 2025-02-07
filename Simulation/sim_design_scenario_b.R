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
between_subject_variances <- c(0.0625, 0.5625) # SD = 0.25, 0.75

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
    group_by(id, Group, sample_size, bs_variance) |> 
    summarise(rating = max(rating_numeric))
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

# Synthpop ----------------------------------------------------------------

# Load necessary libraries
library(synthpop)
library(tibble) # For rownames_to_column function

## Hierarchical ------------------------------------------------------------

# Assuming summarized_datasets is your list of datasets
# Initialize an empty data frame to store the s_pMSE results
pmse_results <- data.frame(dataset_index = integer(), synthetic_index = integer(), s_pMSE = numeric())

# Loop through each dataset in the list
for (i in seq_along(all_datasets)) {
  
  # Select relevant variables and convert to data frame
  original <- all_datasets[[i]] |> 
    dplyr::select(c(id, Group, rating)) |> 
    mutate(rating = case_when(rating == 1 ~ "None",
                              rating == 2 ~ "Mild",
                              rating == 3 ~ "Moderate",
                              rating == 4 ~ "Severe")) |> 
    as.data.frame()
  
  # Generate 100 synthetic datasets for each original dataset
  for (j in 1:100) {
    
    # Generate a synthetic dataset
    synthetic_data <- syn(original)
    
    # Calculate utility with s_pMSE
    utility_df <- utility.tables(synthetic_data, original, tables = "twoway", tab.stats = "S_pMSE", print.tabs = FALSE)
    
    # Extract s_pMSE value
    s_pMSE_value <- utility_df$tabs |> 
      as.data.frame() |> 
      rownames_to_column(var = "rowname") |> 
      dplyr::pull(S_pMSE)
    
    # Check if s_pMSE_value is found
    if (length(s_pMSE_value) == 0) {
      # If no s_pMSE value found, assign NA (or handle accordingly)
      s_pMSE_value <- NA
    }
    
    # Store the dataset index, synthetic dataset index, and s_pMSE value in the results data frame
    pmse_results <- pmse_results |> 
      dplyr::bind_rows(data.frame(dataset_index = i, synthetic_index = j, s_pMSE = s_pMSE_value))
  }
}

# Add descriptors
scenario_b_pmse_results <- pmse_results |> 
  mutate(sample_size = case_when(dataset_index == 1 ~ all_datasets[[1]]$sample_size[1],
                                 dataset_index == 2 ~ all_datasets[[2]]$sample_size[1],
                                 dataset_index == 3 ~ all_datasets[[3]]$sample_size[1],
                                 dataset_index == 4 ~ all_datasets[[4]]$sample_size[1]),
         bs_variance = case_when(dataset_index == 1 ~ all_datasets[[1]]$bs_variance[1],
                                 dataset_index == 2 ~ all_datasets[[2]]$bs_variance[1],
                                 dataset_index == 3 ~ all_datasets[[3]]$bs_variance[1],
                                 dataset_index == 4 ~ all_datasets[[4]]$bs_variance[1])
         )

# Visualize
scenario_b_pmse_results |>
  mutate(name = paste("Sample Size =", sample_size,
                      ", Between-Variability =", bs_variance)) |>
  ggplot(aes(x = s_pMSE)) +
  geom_histogram() +
  scale_x_continuous(limits = c(0, 10)) +
  facet_wrap(~name, nrow = 8) +
  labs(y = "Frequency")

## Non-Hierarchical ------------------------------------------------------------

# Assuming summarized_datasets is your list of datasets
# Initialize an empty data frame to store the s_pMSE results
pmse_results <- data.frame(dataset_index = integer(), synthetic_index = integer(), s_pMSE = numeric())

# Loop through each dataset in the list
for (i in seq_along(summarized_datasets)) {
  
  # Select relevant variables and convert to data frame
  original <- summarized_datasets[[i]] |> 
    dplyr::select(c(Group, rating)) |> 
    dplyr::select(c(id, Group, rating)) |> 
    mutate(rating = case_when(rating == 1 ~ "None",
                              rating == 2 ~ "Mild",
                              rating == 3 ~ "Moderate",
                              rating == 4 ~ "Severe")) |> 
    as.data.frame()
  
  original <- original[, apply(original, 2, function(x) length(unique(x)) > 1)]
  
  # Generate 100 synthetic datasets for each original dataset
  for (j in 1:100) {
    
    # Generate a synthetic dataset
    synthetic_data <- syn(original)
    
    # Calculate utility with s_pMSE
    utility_df <- utility.tables(synthetic_data, original, tables = "twoway", tab.stats = "S_pMSE", print.tabs = FALSE)
    
    # Extract s_pMSE value
    s_pMSE_value <- utility_df$tabs |> 
      as.data.frame() |> 
      rownames_to_column(var = "rowname") |> 
      dplyr::pull(S_pMSE)
    
    # Check if s_pMSE_value is found
    if (length(s_pMSE_value) == 0) {
      # If no s_pMSE value found, assign NA (or handle accordingly)
      s_pMSE_value <- NA
    }
    
    # Store the dataset index, synthetic dataset index, and s_pMSE value in the results data frame
    pmse_results <- pmse_results |> 
      dplyr::bind_rows(data.frame(dataset_index = i, synthetic_index = j, s_pMSE = s_pMSE_value))
  }
}

# Add descriptors
scenario_b_summarized_pmse_results <- pmse_results |> 
  mutate(sample_size = case_when(dataset_index == 1 ~ summarized_datasets[[1]]$sample_size[1],
                                 dataset_index == 2 ~ summarized_datasets[[2]]$sample_size[1],
                                 dataset_index == 3 ~ summarized_datasets[[3]]$sample_size[1],
                                 dataset_index == 4 ~ summarized_datasets[[4]]$sample_size[1]),
         bs_variance = case_when(dataset_index == 1 ~ summarized_datasets[[1]]$bs_variance[1],
                                 dataset_index == 2 ~ summarized_datasets[[2]]$bs_variance[1],
                                 dataset_index == 3 ~ summarized_datasets[[3]]$bs_variance[1],
                                 dataset_index == 4 ~ summarized_datasets[[4]]$bs_variance[1])
  )

# Visualize
scenario_b_summarized_pmse_results |>
  mutate(name = paste("Sample Size =", sample_size,
                      ", Between-Variability =", bs_variance)) |>
  ggplot(aes(x = s_pMSE)) +
  geom_histogram() +
  scale_x_continuous(limits = c(0, 10)) +
  facet_wrap(~name, nrow = 8) +
  labs(y = "Frequency")
