# Load simstudy library
library(simstudy)
library(tidyverse)

# Define the data generation for sessions
defSessions <- defData(varname = "session", dist = "nonrandom", formula = 1, id = "sessionID")
sessionData <- genData(11, defSessions)  # Generate data for 11 sessions
sessionData$session <- 1:11  # Assign session numbers explicitly

# Define the data generation for the number of sets per session (fixed at 5 sets per session)
defSets <- defData(varname = "set", dist = "nonrandom", formula = 5)

# Add the number of sets per session to sessionData
sessionData <- addColumns(defSets, sessionData)

# Define the data generation for IDs within sets (mean number of IDs per set is 3)
defIDs <- defData(varname = "id", dist = "poisson", formula = 3)

# Between-subject variability
between_variation <- defDataAdd(
  varname = "participant_variation",
  formula = 0,
  variance = 0, 
  dist = "normal"
)

sessionData2 <- addColumns(between_variation, sessionData)

# Generate the hierarchical data for sets within sessions
set.seed(123)
sets <- genCluster(sessionData2, "sessionID", numIndsVar = "set", level1ID = "setID")

# Add the number of IDs per set to sets
sets <- addColumns(defIDs, sets)

# Generate the hierarchical data for IDs within sets
ids <- genCluster(sets, "setID", numIndsVar = "id", level1ID = "id")

# Add the binary outcome variable with no change for sessions 1 to 6 and increase from session 7
defResponse <- defDataAdd(
  varname = "response", 
  dist = "binary", 
  formula = "ifelse(session <= 6, -3.75, -1.1 + 0.5)",  # -1.1 corresponds to a probability of about 0.25; increase from session 7
  link = "logit",
  variance = 0
)

final_data <- addColumns(defResponse, ids)

# Inspect the first few rows of the generated data
head(final_data)

# Calculate the proportion of positive responses for each session
summary_data <- final_data |>
  group_by(session) |>
  summarise(proportion_positive = mean(response))

# Create a line plot of the proportion of positive responses over sessions
ggplot(summary_data, aes(x = session, y = proportion_positive)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Proportion of Positive Responses Over Time",
    x = "Session",
    y = "Proportion of Positive Responses"
  ) +
  theme_minimal()

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
    dplyr::select(c(subject_id, time, peak_flow)) |> 
    mutate(time = case_when(time == 0 ~ "Pre",
                            time == 1 ~ "Post"),
           time = as.factor(time)) |> 
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
scenario_a_pmse_results <- pmse_results |> 
  mutate(sample_size = case_when(dataset_index == 1 ~ datasets[[1]]$sample_size[1],
                                 dataset_index == 2 ~ datasets[[2]]$sample_size[1],
                                 dataset_index == 3 ~ datasets[[3]]$sample_size[1],
                                 dataset_index == 4 ~ datasets[[4]]$sample_size[1],
                                 dataset_index == 5 ~ datasets[[5]]$sample_size[1],
                                 dataset_index == 6 ~ datasets[[6]]$sample_size[1],
                                 dataset_index == 7 ~ datasets[[7]]$sample_size[1],
                                 dataset_index == 8 ~ datasets[[8]]$sample_size[1]),
         bs_variance = case_when(dataset_index == 1 ~ datasets[[1]]$bs_variance[1],
                                 dataset_index == 2 ~ datasets[[2]]$bs_variance[1],
                                 dataset_index == 3 ~ datasets[[3]]$bs_variance[1],
                                 dataset_index == 4 ~ datasets[[4]]$bs_variance[1],
                                 dataset_index == 5 ~ datasets[[5]]$bs_variance[1],
                                 dataset_index == 6 ~ datasets[[6]]$bs_variance[1],
                                 dataset_index == 7 ~ datasets[[7]]$bs_variance[1],
                                 dataset_index == 8 ~ datasets[[8]]$bs_variance[1]),
         ws_variance = case_when(dataset_index == 1 ~ datasets[[1]]$ws_variance[1],
                                 dataset_index == 2 ~ datasets[[2]]$ws_variance[1],
                                 dataset_index == 3 ~ datasets[[3]]$ws_variance[1],
                                 dataset_index == 4 ~ datasets[[4]]$ws_variance[1],
                                 dataset_index == 5 ~ datasets[[5]]$ws_variance[1],
                                 dataset_index == 6 ~ datasets[[6]]$ws_variance[1],
                                 dataset_index == 7 ~ datasets[[7]]$ws_variance[1],
                                 dataset_index == 8 ~ datasets[[8]]$ws_variance[1])
  )

# Visualize
scenario_a_pmse_results |>
  mutate(name = paste("Sample Size =", sample_size,
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
    dplyr::select(c(time, peak_flow)) |> 
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
scenario_a_summarized_pmse_results <- pmse_results |> 
  mutate(sample_size = case_when(dataset_index == 1 ~ summarized_datasets[[1]]$sample_size[1],
                                 dataset_index == 2 ~ summarized_datasets[[2]]$sample_size[1],
                                 dataset_index == 3 ~ summarized_datasets[[3]]$sample_size[1],
                                 dataset_index == 4 ~ summarized_datasets[[4]]$sample_size[1],
                                 dataset_index == 5 ~ summarized_datasets[[5]]$sample_size[1],
                                 dataset_index == 6 ~ summarized_datasets[[6]]$sample_size[1],
                                 dataset_index == 7 ~ summarized_datasets[[7]]$sample_size[1],
                                 dataset_index == 8 ~ summarized_datasets[[8]]$sample_size[1]),
         bs_variance = case_when(dataset_index == 1 ~ summarized_datasets[[1]]$bs_variance[1],
                                 dataset_index == 2 ~ summarized_datasets[[2]]$bs_variance[1],
                                 dataset_index == 3 ~ summarized_datasets[[3]]$bs_variance[1],
                                 dataset_index == 4 ~ summarized_datasets[[4]]$bs_variance[1],
                                 dataset_index == 5 ~ summarized_datasets[[5]]$bs_variance[1],
                                 dataset_index == 6 ~ summarized_datasets[[6]]$bs_variance[1],
                                 dataset_index == 7 ~ summarized_datasets[[7]]$bs_variance[1],
                                 dataset_index == 8 ~ summarized_datasets[[8]]$bs_variance[1]),
         ws_variance = case_when(dataset_index == 1 ~ summarized_datasets[[1]]$ws_variance[1],
                                 dataset_index == 2 ~ summarized_datasets[[2]]$ws_variance[1],
                                 dataset_index == 3 ~ summarized_datasets[[3]]$ws_variance[1],
                                 dataset_index == 4 ~ summarized_datasets[[4]]$ws_variance[1],
                                 dataset_index == 5 ~ summarized_datasets[[5]]$ws_variance[1],
                                 dataset_index == 6 ~ summarized_datasets[[6]]$ws_variance[1],
                                 dataset_index == 7 ~ summarized_datasets[[7]]$ws_variance[1],
                                 dataset_index == 8 ~ summarized_datasets[[8]]$ws_variance[1])
  )

# Visualize
scenario_a_summarized_pmse_results |>
  mutate(name = paste("Sample Size =", sample_size,
                      ", Between-Variability =", bs_variance,
                      ", Within-Variability =", ws_variance)) |>
  ggplot(aes(x = s_pMSE)) +
  geom_histogram() +
  scale_x_continuous(limits = c(0, 10)) +
  facet_wrap(~name, nrow = 8) +
  labs(y = "Frequency")
