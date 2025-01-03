#R script for analysis of tDCS data - analysis of language scores obtained by a 
#person with aphasia at 5 assessment times

setwd("address of the folder containing the csv data files")

#Load packages
library(tidyverse)

#Load and rename data files separately for analyses of overall and subtest scores as a function of time and change/difference
tdcs_data <- read_csv(here::here("Data", "07_Language", "Aydin", "raw_data", "data_singlecol.csv"))
tdcs_data_audcomprehension <- read_csv("data_singlecol_audcomprehension.csv")
tdcs_data_repetition <- read_csv("data_singlecol_repetition.csv")
tdcs_data_naming <- read_csv("data_singlecol_naming.csv")
tdcs_data_grammar <- read_csv("data_singlecol_grammar.csv")
tdcs_data_reading <- read_csv("data_singlecol_reading.csv")
tdcs_data_pragmatics <- read_csv("data_singlecol_pragmatics.csv")
tdcs_data_diff <- read_csv("data_difference.csv")
tdcs_data_diff_audcomprehension <- read_csv("data_difference_audcomprehension.csv")
tdcs_data_diff_repetition <- read_csv("data_difference_repetition.csv")
tdcs_data_diff_naming <- read_csv("data_difference_naming.csv")
tdcs_data_diff_grammar <- read_csv("data_difference_grammar.csv")
tdcs_data_diff_reading <- read_csv("data_difference_reading.csv")
tdcs_data_diff_pragmatics <- read_csv("data_difference_pragmatics.csv")

############## Comparison of Language Scores Across Assessment Times #############
#get mean & median scores for each time point
aggregate(tdcs_data["scores"], list(tdcs_data$time), mean)
aggregate(tdcs_data["scores"], list(tdcs_data$time), median)

#Friedman test on overall language scores at 5 time points
friedman.test(y=tdcs_data$scores, groups=tdcs_data$time, blocks=tdcs_data$item)

#posthoc wilcoxon tests with Bonferroni correction
pairwise.wilcox.test(tdcs_data$scores, tdcs_data$time, p.adj = "bonf")

#Friedman tests on subtest scores
friedman.test(y=tdcs_data_audcomprehension$scores, groups=tdcs_data_audcomprehension$time, blocks=tdcs_data_audcomprehension$item)
friedman.test(y=tdcs_data_repetition$scores, groups=tdcs_data_repetition$time, blocks=tdcs_data_repetition$item)
friedman.test(y=tdcs_data_naming$scores, groups=tdcs_data_naming$time, blocks=tdcs_data_naming$item)
friedman.test(y=tdcs_data_grammar$scores, groups=tdcs_data_grammar$time, blocks=tdcs_data_grammar$item)
friedman.test(y=tdcs_data_reading$scores, groups=tdcs_data_reading$time, blocks=tdcs_data_reading$item)
friedman.test(y=tdcs_data_pragmatics$scores, groups=tdcs_data_pragmatics$time, blocks=tdcs_data_pragmatics$item)

#get mean and median scores for each time point for each subtest
aggregate(tdcs_data_audcomprehension["scores"], list(tdcs_data_audcomprehension$time), mean)
aggregate(tdcs_data_repetition["scores"], list(tdcs_data_repetition$time), mean)
aggregate(tdcs_data_naming["scores"], list(tdcs_data_naming$time), mean)
aggregate(tdcs_data_grammar["scores"], list(tdcs_data_grammar$time), mean)
aggregate(tdcs_data_reading["scores"], list(tdcs_data_reading$time), mean)
aggregate(tdcs_data_pragmatics["scores"], list(tdcs_data_pragmatics$time), mean)

aggregate(tdcs_data_audcomprehension["scores"], list(tdcs_data_audcomprehension$time), median)
aggregate(tdcs_data_repetition["scores"], list(tdcs_data_repetition$time), median)
aggregate(tdcs_data_naming["scores"], list(tdcs_data_naming$time), median)
aggregate(tdcs_data_grammar["scores"], list(tdcs_data_grammar$time), median)
aggregate(tdcs_data_reading["scores"], list(tdcs_data_reading$time), median)
aggregate(tdcs_data_pragmatics["scores"], list(tdcs_data_pragmatics$time), median)

#posthoc wilcoxon tests
pairwise.wilcox.test(tdcs_data_audcomprehension$scores, tdcs_data_audcomprehension$time, p.adj = "bonf")
pairwise.wilcox.test(tdcs_data_repetition$scores, tdcs_data_repetition$time, p.adj = "bonf")
pairwise.wilcox.test(tdcs_data_naming$scores, tdcs_data_naming$time, p.adj = "bonf")
pairwise.wilcox.test(tdcs_data_grammar$scores, tdcs_data_grammar$time, p.adj = "bonf")
pairwise.wilcox.test(tdcs_data_reading$scores, tdcs_data_reading$time, p.adj = "bonf")
pairwise.wilcox.test(tdcs_data_pragmatics$scores, tdcs_data_pragmatics$time, p.adj = "bonf")

######## Comparison of Change in Scores Associated with Treatments #############

#comparison of difference scores (posttests - pretest) for overall scores
#get mean and median post-pre difference scores for each phase
aggregate(tdcs_data_diff["post_minus_pre_scores"], list(tdcs_data_diff$phase), mean)
aggregate(tdcs_data_diff["post_minus_pre_scores"], list(tdcs_data_diff$phase), median)

pairwise.wilcox.test(tdcs_data_diff$post_minus_pre_scores, tdcs_data_diff$phase, p.adj = "bonf")

#comparison of difference scores (posttests - pretest) for subtest scores
#get mean and median post-pre difference scores for each phase for each subtest
aggregate(tdcs_data_diff_audcomprehension["post_minus_pre_scores"], list(tdcs_data_diff_audcomprehension$phase), mean)
aggregate(tdcs_data_diff_repetition["post_minus_pre_scores"], list(tdcs_data_diff_repetition$phase), mean)
aggregate(tdcs_data_diff_naming["post_minus_pre_scores"], list(tdcs_data_diff_naming$phase), mean)
aggregate(tdcs_data_diff_grammar["post_minus_pre_scores"], list(tdcs_data_diff_grammar$phase), mean)
aggregate(tdcs_data_diff_reading["post_minus_pre_scores"], list(tdcs_data_diff_reading$phase), mean)
aggregate(tdcs_data_diff_pragmatics["post_minus_pre_scores"], list(tdcs_data_diff_pragmatics$phase), mean)

aggregate(tdcs_data_diff_audcomprehension["post_minus_pre_scores"], list(tdcs_data_diff_audcomprehension$phase), median)
aggregate(tdcs_data_diff_repetition["post_minus_pre_scores"], list(tdcs_data_diff_repetition$phase), median)
aggregate(tdcs_data_diff_naming["post_minus_pre_scores"], list(tdcs_data_diff_naming$phase), median)
aggregate(tdcs_data_diff_grammar["post_minus_pre_scores"], list(tdcs_data_diff_grammar$phase), median)
aggregate(tdcs_data_diff_reading["post_minus_pre_scores"], list(tdcs_data_diff_reading$phase), median)
aggregate(tdcs_data_diff_pragmatics["post_minus_pre_scores"], list(tdcs_data_diff_pragmatics$phase), median)

pairwise.wilcox.test(tdcs_data_diff_audcomprehension$post_minus_pre_scores, tdcs_data_diff_audcomprehension$phase, p.adj = "bonf")
pairwise.wilcox.test(tdcs_data_diff_repetition$post_minus_pre_scores, tdcs_data_diff_repetition$phase, p.adj = "bonf")
pairwise.wilcox.test(tdcs_data_diff_naming$post_minus_pre_scores, tdcs_data_diff_naming$phase, p.adj = "bonf")
pairwise.wilcox.test(tdcs_data_diff_grammar$post_minus_pre_scores, tdcs_data_diff_grammar$phase, p.adj = "bonf")
pairwise.wilcox.test(tdcs_data_diff_reading$post_minus_pre_scores, tdcs_data_diff_reading$phase, p.adj = "bonf")
pairwise.wilcox.test(tdcs_data_diff_pragmatics$post_minus_pre_scores, tdcs_data_diff_pragmatics$phase, p.adj = "bonf")

