library(purrr)
library(kableExtra)
library(tidybayes)

# Original Data Import & Preprocessing
df <- read.csv(here::here("Data", "07_Language", "Robinaugh", 'SDTBI001_Data.csv')) %>%
  janitor::clean_names() %>%
  mutate(condition = ifelse(condition == 0, 'untreated', 'treated'),
         phase = as.factor(phase)) %>%
  mutate_if(is.character, as.factor)

# Prepare original model data
df_coded2 <- df %>%
  filter(session < 13) %>%
  group_by(id) %>%
  mutate(phase = ifelse(phase == 'Pre', 0, 1),
         num_baselines = sum(phase == 0),
         slope_change = (session - (num_baselines + 1)) * phase) %>%
  select(id, set, condition, response, session, phase, slope_change) %>%
  mutate(phase = ifelse(condition == "untreated", 0, phase),
         slope_change = ifelse(condition == "untreated", 0, slope_change))

# Analysis Function
get_model_estimate <- function(model) {
  pred_d <- model$data %>%
    select(-response, -Intercept) %>%
    group_by(id) %>%
    mutate(num_baselines = ifelse(set != 7, sum(phase == 0), 4)) %>%
    filter(session == num_baselines | session == max(session))
  
  es <- add_epred_draws(model, newdata = pred_d |> filter(set != 7), seed = 42) %>%
    ungroup() %>%
    mutate(time_point = ifelse(slope_change == 0, 'entry', 'exit')) %>%
    select(time_point, id, value = .epred, draw = .draw) %>%
    group_by(draw, time_point) %>%
    summarize(num_corr = sum(value), .groups = "drop") %>%
    pivot_wider(names_from = time_point, values_from = num_corr) %>%
    mutate(effect_size = exit - entry) %>%
    select(effect_size)
  
  median_qi(es$effect_size)
}

# Original model saving/loading
original_model_path <- here::here("Data", "07_Language", "Robinaugh", "original_model.rds")

if (file.exists(original_model_path)) {
  original_model <- readRDS(original_model_path)
} else {
  original_model <- brm(response ~ 0 + Intercept + session + phase + slope_change +
                          (session + phase + slope_change | set/id),
                        family = bernoulli(),
                        data = df_coded2,
                        iter = 4000, warmup = 1000,
                        chains = 4, cores = 4,
                        control = list(adapt_delta = .97),
                        prior = c(prior(normal(-1, 2), class = b, coef = Intercept),
                                  prior(normal(0, 2), class = b),
                                  prior(normal(0, 2), class = sd),
                                  prior(lkj(2), class = cor)),
                        backend = 'cmdstan', seed = 42, refresh = 0)
  saveRDS(original_model, original_model_path)
}

# Original estimate
original_estimate <- get_model_estimate(original_model)

# Generate synthetic data
syn_data <- syn(df_coded2, m = 7, seed = 2024)

# Analyze Synthetic Datasets
synthetic_results <- syn_data$syn %>%
  imap_dfr(~ {
    synthetic_model_path <- here::here("Data", "07_Language", "Robinaugh", "synthetic_models", paste0("synthetic_model_", .y, ".rds"))
    
    if (file.exists(synthetic_model_path)) {
      synthetic_model <- readRDS(synthetic_model_path)
    } else {
      synthetic_model <- brm(response ~ 0 + Intercept + session + phase + slope_change +
                               (session + phase + slope_change | set/id),
                             family = bernoulli(),
                             data = .x,
                             iter = 2000, warmup = 1000,
                             chains = 2, cores = 2,
                             control = list(adapt_delta = .97),
                             prior = c(prior(normal(-1, 2), class = b, coef = Intercept),
                                       prior(normal(0, 2), class = b),
                                       prior(normal(0, 2), class = sd),
                                       prior(lkj(2), class = cor)),
                             backend = 'cmdstan', seed = 42, refresh = 0)
      dir.create(dirname(synthetic_model_path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(synthetic_model, synthetic_model_path)
    }
    
    synthetic_estimate <- get_model_estimate(synthetic_model)
    
    tibble(
      synthetic_effect_size = synthetic_estimate$y,
      synthetic_lower = synthetic_estimate$ymin,
      synthetic_upper = synthetic_estimate$ymax
    )
  }, .id = "dataset")

# Calculate differences
synthetic_results <- synthetic_results %>%
  mutate(
    diff_effect_size = synthetic_effect_size - original_estimate$y,
    diff_CI_lower = synthetic_lower - original_estimate$ymin,
    diff_CI_upper = synthetic_upper - original_estimate$ymax
  )

# Summarize synthetic results
results_summary <- tibble(
  effect_size_original = original_estimate$y,
  CI_lower_original = original_estimate$ymin,
  CI_upper_original = original_estimate$ymax,
  mean_diff_effect_size = mean(synthetic_results$diff_effect_size),
  sd_diff_effect_size = sd(synthetic_results$diff_effect_size),
  mean_diff_CI_lower = mean(synthetic_results$diff_CI_lower),
  mean_diff_CI_upper = mean(synthetic_results$diff_CI_upper)
)

# Save results
saveRDS(list(
  original_model = original_model,
  synthetic_results = synthetic_results,
  summary = results_summary
), here::here("Data", "07_Language", "Robinaugh", "analysisDataBayesian.RDS"))