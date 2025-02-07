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
between_subject_variances <- c(0.25, 0.75)
within_subject_variances <- c(0.25, 0.75)

# Create a grid of all combinations of the parameters
param_grid <- expand.grid(
  bs_variance = between_subject_variances,
  ws_variance = within_subject_variances
)

# Function to generate data based on parameters
generate_single_subject_data <- function(bs_variance, ws_variance) {
  
  # Define the data generation for sessions (15 sessions total)
  defSessions <- defData(varname = "session", dist = "nonrandom", formula = 1, id = "sessionID")
  sessionData <- genData(15, defSessions)  # Generate data for 15 sessions
  sessionData$session <- 1:15  # Assign session numbers explicitly
  
  # Label sessions as 'baseline' (1-5) or 'tx' (6-15)
  sessionData$phase <- ifelse(sessionData$session <= 5, "baseline", "tx")
  
  # Generate item-level characteristics (60 items)
  defItems <- defData(varname = "item_type", dist = "categorical", formula = "0.5;0.5")  # 50% Treated, 50% Untreated
  defItemVariability <- defDataAdd(
    varname = "item_variability", 
    dist = "normal", 
    formula = 0, 
    variance = bs_variance  # Item-level variability
  )
  
  # Generate 60 items (30 Treated and 30 Untreated) with item type and variability
  items <- genData(60)  # 60 items total
  items <- addColumns(defItems, items)  # Add item_type to items
  items <- addColumns(defItemVariability, items)  # Add item_variability to items
  
  # Label items as 'Treated' or 'Untreated'
  items$item_type <- ifelse(items$item_type == 1, "Treated", "Untreated")
  
  # Now replicate the items for all sessions, maintaining order (1-60 for each session)
  items_all_sessions <- items[rep(1:nrow(items), times = 15), ]  # Replicate items 1-60 for each session
  items_all_sessions$session <- rep(sessionData$session, each = 60)  # Add session numbers to each replicated item
  
  # Define the binary response with low response (5-10%) for sessions 1-5
  # After session 5, treated shows a sharp increase and continues to rise; untreated shows no increase
  defResponse <- defDataAdd(
    varname = "response", 
    dist = "binary", 
    formula = "ifelse(session <= 5, 
                   -2.25 + item_variability, 
                   ifelse(item_type == 'Treated', 
                          -0.25 + 0.15 * (session - 5) + item_variability, 
                          -1 + 0.08 * (session - 5) + item_variability))", 
    # Flat response for sessions 1-5; gradual increase from session 6 for Treated and Untreated
    link = "logit",
    variance = 0
  )
  
  # Add the response data to the items
  final_data <- addColumns(defResponse, items_all_sessions)
  
  # Add columns for between-subject variance and within-subject variance
  dd <- final_data |>
    mutate(
      bs_variance = bs_variance,
      ws_variance = ws_variance
    )
  
  return(dd)
}

# Generate datasets using the parameter grid
datasets <- param_grid |>
  pmap(function(bs_variance, ws_variance) {
    generate_single_subject_data(bs_variance, ws_variance)
  })

# Name the datasets for clarity
names(datasets) <- paste0("_bs", param_grid$bs_variance, 
                          "_ws", param_grid$ws_variance)

### Export to Excel ---------------------------------------------------------

save_datasets_to_csv <- function(datasets, param_grid) {
  # Loop through datasets
  for (i in 1:length(datasets)) {
    # Get dataset parameters
    bs_variance <- param_grid$bs_variance[i]
    ws_variance <- param_grid$ws_variance[i]
    
    # Create the dataset name for the file
    dataset_name <- paste0("single_subject", "_bs", bs_variance, "_ws", ws_variance)
    
    # Save the hierarchical dataset
    write.csv(datasets[[i]], here::here("Simulation", "Scenario_C", "Hierarchical", paste0(dataset_name, "_hierarchical.csv")), row.names = FALSE)
  }
}

# Call the function to save the datasets to CSV files
save_datasets_to_csv(datasets, param_grid)

## Non-Hierarchical Dataset ----------------------------------------------------

# Function to summarize each dataset
summarize_dataset <- function(data) {
  data |>
    group_by(session, item_type, bs_variance, ws_variance) %>%
    summarise(proportion = mean(response)) |> 
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
    bs_variance <- param_grid$bs_variance[i]
    ws_variance <- param_grid$ws_variance[i]
    
    # Create the dataset name for the file
    dataset_name <- paste0("summarized_single_subject", "_bs", bs_variance, "_ws", ws_variance)
    
    # Define the output file path
    output_file <- here::here("Simulation", "Scenario_A", "Non_Hierarchical", paste0(dataset_name, "_summarized.csv"))
    
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
for (i in seq_along(datasets)) {
  
  # Select relevant variables and convert to data frame
  original <- datasets[[i]] |> 
    dplyr::select(c(id, item_type, session, response)) |> 
    mutate(response = case_when(response == 0 ~ "Incorrect", response == 1 ~ "Correct")) |> 
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
scenario_c_pmse_results <- pmse_results |> 
  mutate(bs_variance = case_when(dataset_index == 1 ~ datasets[[1]]$bs_variance[1],
                                 dataset_index == 2 ~ datasets[[2]]$bs_variance[1],
                                 dataset_index == 3 ~ datasets[[3]]$bs_variance[1],
                                 dataset_index == 4 ~ datasets[[4]]$bs_variance[1]),
         ws_variance = case_when(dataset_index == 1 ~ datasets[[1]]$ws_variance[1],
                                 dataset_index == 2 ~ datasets[[2]]$ws_variance[1],
                                 dataset_index == 3 ~ datasets[[3]]$ws_variance[1],
                                 dataset_index == 4 ~ datasets[[4]]$ws_variance[1])
  )

# Visualize
scenario_c_pmse_results |>
  mutate(name = paste("Single Subject",
                      ", Between-Variability =", bs_variance,
                      ", Within-Variability =", ws_variance)) |>
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
    dplyr::select(c(session, proportion)) |> 
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
scenario_c_summarized_pmse_results <- pmse_results |> 
  mutate(bs_variance = case_when(dataset_index == 1 ~ summarized_datasets[[1]]$bs_variance[1],
                                 dataset_index == 2 ~ summarized_datasets[[2]]$bs_variance[1],
                                 dataset_index == 3 ~ summarized_datasets[[3]]$bs_variance[1],
                                 dataset_index == 4 ~ summarized_datasets[[4]]$bs_variance[1]),
         ws_variance = case_when(dataset_index == 1 ~ summarized_datasets[[1]]$ws_variance[1],
                                 dataset_index == 2 ~ summarized_datasets[[2]]$ws_variance[1],
                                 dataset_index == 3 ~ summarized_datasets[[3]]$ws_variance[1],
                                 dataset_index == 4 ~ summarized_datasets[[4]]$ws_variance[1])
  )

# Visualize
scenario_c_summarized_pmse_results |>
  mutate(name = paste("Single Subject",
                      ", Between-Variability =", bs_variance,
                      ", Within-Variability =", ws_variance)) |>
  ggplot(aes(x = s_pMSE)) +
  geom_histogram() +
  scale_x_continuous(limits = c(0, 10)) +
  facet_wrap(~name, nrow = 8) +
  labs(y = "Frequency")

