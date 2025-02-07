# Load simstudy and tidyverse libraries
library(simstudy)
library(tidyverse)

set.seed(2024)

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
  variance = 0.0625  # Item-level variability (you can adjust the variance here)
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
# After session 5, treated shows a sharp increase and continues to rise; untreated increases slowly
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

# Inspect the first few rows of the generated data
head(final_data)

# Calculate the proportion of positive responses for each session and item type
summary_data <- final_data %>%
  group_by(session, item_type) %>%
  summarise(proportion_positive = mean(response))

# Create a line plot of the proportion of positive responses over sessions
ggplot(summary_data, aes(x = session, y = proportion_positive, group = item_type, color = item_type)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Proportion of Positive Responses Over Time",
    x = "Session",
    y = "Proportion of Positive Responses",
    color = "Item Type"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1))


