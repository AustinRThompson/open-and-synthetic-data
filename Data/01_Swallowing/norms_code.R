# Load Packages -----------------------------------------------------------
library(janitor)         # used to clean variable names after loading dataset
library(tidyverse)       # ggplot, dplyr, %>%, and friends
library(irr)             # Used for reliability analysis
library(table1)           # Used to quickly and easily create summary/demographic tables

#Data Visualization Packages
library(cowplot)        # theme for plots
library(ggbreak)        # used to create axis breaks
library(ggeasy)


# Intra-Rater Reliability Analysis -------------------------------------------------------------
#Load data
intra_reliability_data <- read.csv(here::here("norms_intrarater.csv"))

#Clean data
intra_reliability_data <- janitor::clean_names(intra_reliability_data)

#Now calculate the reliability coefficients and decsriptive data
#Kappas and absolute agreement calculated for all categorical data
#ICCs and average absolute difference calculated for all continuous data

#Number of Swallows
number <- kappa2(intra_reliability_data[, c(16, 17)], weight = c("equal"), sort.levels = FALSE)
number
agree(intra_reliability_data[, c(16, 17)], tolerance=0)

#Oropharyngeal Residue Rating
oropharynx <- irr::icc(intra_reliability_data[, c(19, 20)], 
                       model = "twoway", 
                       type = "agreement", 
                       unit = "single")
oropharynx

intra_reliability_data %>%
  mutate(oro_dif = abs(x1_oropharynx - x2_oropharynx)) %>%
  dplyr::summarize(average_oro = mean(oro_dif))

#Hypopharyngeal Residue Rating
hypopharynx <- irr::icc(intra_reliability_data[, c(22, 23)], 
                        model = "twoway", 
                        type = "agreement", 
                        unit = "single")
hypopharynx

intra_reliability_data %>%
  mutate(hypo_dif = abs(x1_hypopharynx - x2_hypopharynx)) %>%
  dplyr::summarize(average_hypo = mean(hypo_dif))

#Epiglottic Residue Rating
epiglottis <- irr::icc(intra_reliability_data[, c(25, 26)], 
                       model = "twoway", 
                       type = "agreement", 
                       unit = "single")
epiglottis

intra_reliability_data %>%
  mutate(epiglottis_dif = abs(x1_epiglottis - x2_epiglottis)) %>%
  dplyr::summarize(average_epiglottis = mean(epiglottis_dif))

#Laryngeal Vestibule Residue Rating
vestibule <- irr::icc(intra_reliability_data[, c(28, 29)], 
                      model = "twoway", 
                      type = "agreement", 
                      unit = "single")
vestibule

intra_reliability_data %>%
  mutate(vestibule_dif = abs(x1_vocal_folds - x2_vestibule)) %>%
  dplyr::summarize(average_vestibule = mean(vestibule_dif))

#Vocal Fold Residue Rating
VF <- irr::icc(intra_reliability_data[, c(31, 32)], 
               model = "twoway", 
               type = "agreement", 
               unit = "single")
VF

intra_reliability_data %>%
  mutate(vocal_folds_dif = abs(x1_vocal_folds - x2_vocal_folds)) %>%
  dplyr::summarize(average_vocal_folds = mean(vocal_folds_dif))

#Subglottic Residue Rating
SG <- irr::icc(intra_reliability_data[, c(34, 35)], 
               model = "twoway", 
               type = "agreement", 
               unit = "single")
SG

intra_reliability_data %>%
  mutate(subglottis_dif = abs(x1_subglottis - x2_subglottis)) %>%
  dplyr::summarize(average_subglottis = mean(subglottis_dif))

#Penetration-Aspiration Scale (PAS) ratings
PAS <- kappa2(intra_reliability_data[, c(37, 38)], weight = c("squared"), sort.levels = FALSE)
PAS

agree(intra_reliability_data[, c(37, 38)], tolerance=0)

#Bolus Location at Swallow Onset: Oral Cavity Only
onset_oral <- irr::kappam.fleiss(intra_reliability_data[, c(40, 41)])
onset_oral

agree(intra_reliability_data[, c(40, 41)], tolerance=0)

# Inter-Rater Reliability -------------------------------------------------
#Load data
inter_reliability_data <- read.csv(here::here("norms_interrater.csv"))

#Clean data
inter_reliability_data <- janitor::clean_names(inter_reliability_data)

#Now calculate the reliability coefficients and decsriptive data
#Kappas and absolute agreement calculated for all categorical data
#ICCs and average absolute difference calculated for all continuous data

#Number of Swallows
number <- kappa2(inter_reliability_data[, c(16, 17)], weight = c("equal"), sort.levels = FALSE)
number
agree(inter_reliability_data[, c(16, 17)], tolerance=0)

#Oropharyngeal Residue Rating
oropharynx <- irr::icc(inter_reliability_data[, c(19, 20)], 
                       model = "twoway", 
                       type = "agreement", 
                       unit = "single")
oropharynx

inter_reliability_data %>%
  mutate(oro_dif = abs(x1_oropharynx - x2_oropharynx)) %>%
  dplyr::summarize(average_oro = mean(oro_dif))


oropharynx <- irr::icc(intra_reliability_data[, c(19, 20)], 
                       model = "twoway", 
                       type = "agreement", 
                       unit = "single")
#Hypopharyngeal Residue Rating
hypopharynx <- irr::icc(inter_reliability_data[, c(22, 23)], 
                        model = "twoway", 
                        type = "agreement", 
                        unit = "single")
hypopharynx

inter_reliability_data %>%
  mutate(hypo_dif = abs(x1_hypopharynx - x2_hypopharynx)) %>%
  dplyr::summarize(average_hypo = mean(hypo_dif))

#Epiglottic Residue Rating
epiglottis <- irr::icc(inter_reliability_data[, c(25, 26)], 
                       model = "twoway", 
                       type = "agreement", 
                       unit = "single")
epiglottis

inter_reliability_data %>%
  mutate(epiglottis_dif = abs(x1_epiglottis - x2_epiglottis)) %>%
  dplyr::summarize(average_epiglottis = mean(epiglottis_dif))

#Laryngeal Vestibule Residue Rating
vestibule <- irr::icc(inter_reliability_data[, c(28, 29)], 
                      model = "twoway", 
                      type = "agreement", 
                      unit = "single")
vestibule

inter_reliability_data %>%
  mutate(vestibule_dif = abs(x1_vocal_folds - x2_vestibule)) %>%
  dplyr::summarize(average_vestibule = mean(vestibule_dif))

#Vocal Fold Residue Rating
VF <- irr::icc(inter_reliability_data[, c(31, 32)], 
               model = "twoway", 
               type = "agreement", 
               unit = "single")
VF

inter_reliability_data %>%
  mutate(vocal_folds_dif = abs(x1_vocal_folds - x2_vocal_folds)) %>%
  dplyr::summarize(average_vocal_folds = mean(vocal_folds_dif))

#Subglottic Residue Rating
SG <- irr::icc(inter_reliability_data[, c(34, 35)], 
               model = "twoway", 
               type = "agreement", 
               unit = "single")
SG

inter_reliability_data %>%
  mutate(subglottis_dif = abs(x1_subglottis - x2_subglottis)) %>%
  dplyr::summarize(average_subglottis = mean(subglottis_dif))

#Penetration-Aspiration Scale (PAS) ratings
PAS <- kappa2(inter_reliability_data[, c(37, 38)], weight = c("squared"), sort.levels = FALSE)
PAS

agree(inter_reliability_data[, c(37, 38)], tolerance=0)

#Bolus Location at Swallow Onset: Oral Cavity Only
onset_oral <- irr::kappam.fleiss(inter_reliability_data[, c(40, 41)])
onset_oral

agree(inter_reliability_data[, c(40, 41)], tolerance=0)

# Demographics ------------------------------------------------------
#Open dataset of demographic data not available due to IRB restrictions, though code is provided for transparency

#Load data
participants <- read.csv(here::here(""))

#Clean data
participants <- janitor::clean_names(participants)

#Create labels for demographic table
table1::label(participants$age_years) <- "Age (years)"
table1::label(participants$height_inches) <- "Height (inches)"
table1::label(participants$weight_lbs) <- "Weight (pounds)"
table1::label(participants$bmi) <- "Body Mass Index"
table1::label(participants$sex) <- "Sex"
table1::label(participants$gender) <- "Gender"
table1::label(participants$race_ethnicity) <- "Race/Ethnicity"

#Create demographics table
table1::table1(~age_years + height_inches + weight_lbs + bmi + sex + gender + race_ethnicity, 
               data = participants,  
               overall="Total",
               render.continuous=c(.="Mean (SD)", 
                                   .="Median [Q1, Q3]",
                                   .="Min, Max"))

# Data Wrangling -------------------------------------------------------
#Load data
data <- read.csv(here::here("norms_ratings.csv"))

#Clean data
data <- janitor::clean_names(data)

#Examine the data type of each variable
glimpse(data)
structure(data)
head(data)
str(data)
summary(data)

#Change variables into accurate data types
data$population <- factor(data$population, order = FALSE)

data$bolus_colorant <- factor(data$bolus_colorant, order = FALSE)
data$bolus_consistency <- factor(data$bolus_consistency, order = FALSE)
data$bolus_volume <- factor(data$bolus_volume, order = TRUE)
data$bolus_delivery_method <- factor(data$bolus_delivery_method, order = FALSE)
data$instruction_normal_held_cued <- factor(data$instruction_normal_held_cued, order = FALSE)
data$instruction_single_natural_uninterrupted_fast <- factor(data$instruction_single_natural_uninterrupted_fast, order = FALSE)

data$number_of_swallows_performed <- factor(data$number_of_swallows_performed, order = TRUE)

data$pas_before_the_swallow <- factor(data$pas_before_the_swallow, c("1", "2", "3", "4", "5", "6", "7", "8"), order = TRUE)
data$pas_during_the_swallow <- factor(data$pas_during_the_swallow, c("1", "2", "3", "4", "5", "6", "7", "8"), order = TRUE)
data$pas_after_the_swallow <- factor(data$pas_after_the_swallow, c("1", "2", "3", "4", "5", "6", "7", "8"), order = TRUE)
data$pas_between_bolus_trials <- factor(data$pas_between_bolus_trials, c("1", "2", "3", "4", "5", "6", "7", "8"), order = TRUE)
data$pas_max <- factor(data$pas_max, c("1", "2", "3", "4", "5", "6", "7", "8"), order = TRUE)

data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset <- factor(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset, order = FALSE)
data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx <- factor(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx, order = FALSE)
data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis <- factor(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis, order = FALSE)
data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx <- factor(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx, order = FALSE)
data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule <- factor(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule, order = FALSE)
data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds <- factor(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds, order = FALSE)
data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis <- factor(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis, order = FALSE)

data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset <- recode(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset, Checked = "Present", Unchecked = "Absent")
data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx <- recode(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx, Checked = "Present", Unchecked = "Absent")
data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis <- recode(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis, Checked = "Present", Unchecked = "Absent")
data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx <- recode(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx, Checked = "Present", Unchecked = "Absent")
data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule <- recode(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule, Checked = "Present", Unchecked = "Absent")
data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds <- recode(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds, Checked = "Present", Unchecked = "Absent")
data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis <- recode(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis, Checked = "Present", Unchecked = "Absent")

#Create variables for residue that is 'present' vs 'absent'
#This is used for the tables/descriptives AND for color/filling the figures
data <- dplyr::mutate(data, oro_zero = if_else(valleculae_severity_rating == 0, "Absent", "Present"))
data <- dplyr::mutate(data, hypo_zero = if_else(piriform_sinus_severity_rating == 0,"Absent", "Present"))
data <- dplyr::mutate(data, epi_zero = if_else(epiglottis_severity_rating == 0, "Absent", "Present"))
data <- dplyr::mutate(data, lv_zero = if_else(laryngeal_vestibule_severity_rating == 0, "Absent", "Present"))
data <- dplyr::mutate(data, vf_zero = if_else(vocal_folds_severity_rating == 0, "Absent", "Present"))
data <- dplyr::mutate(data, sg_zero = if_else(subglottis_severity_rating == 0, "Absent", "Present"))
data <- dplyr::mutate(data, pas_zero = if_else(pas_max < 2, "Absent", "Present"))

#Create variables for 'amount of residue when present'
data <- dplyr::mutate(data, oro_present = ifelse(valleculae_severity_rating > 0, valleculae_severity_rating, NA))
data <- dplyr::mutate(data, hypo_present = ifelse(piriform_sinus_severity_rating > 0, piriform_sinus_severity_rating, NA))
data <- dplyr::mutate(data, epi_present = ifelse(epiglottis_severity_rating > 0, epiglottis_severity_rating, NA))
data <- dplyr::mutate(data, lv_present = ifelse(laryngeal_vestibule_severity_rating > 0, laryngeal_vestibule_severity_rating, NA))
data <- dplyr::mutate(data, vf_present = ifelse(vocal_folds_severity_rating > 0, vocal_folds_severity_rating, NA))
data <- dplyr::mutate(data, sg_present = ifelse(subglottis_severity_rating > 0, subglottis_severity_rating, NA))

#VASES Norms: Descriptive Statistics  -----------------------------------------------------
#Create labels for tables
table1::label(data$bolus_volume_continuous_scale) <- "Sip/Bite Size"
table1::label(data$number_of_swallows_performed) <- "Number of Swallows"
table1::label(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset) <- "Bolus Location at Swallow Onset: Oral Cavity Only"
table1::label(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx) <- "Bolus Location at Swallow Onset: Oropharynx"
table1::label(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx) <- "Bolus Location at Swallow Onset: Hypopharynx"
table1::label(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis) <- "Bolus Location at Swallow Onset: Epiglottis"
table1::label(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule) <- "Bolus Location at Swallow Onset: Laryngeal Vestibule"
table1::label(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds) <- "Bolus Location at Swallow Onset: Vocal Folds"
table1::label(data$bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis) <- "Bolus Location at Swallow Onset: Subglottis"

table1::label(data$valleculae_severity_rating) <- "Oropharyngeal Residue"
table1::label(data$oro_zero) <- "Oropharyngeal Residue: Present vs. Absent"
table1::label(data$oro_present) <- "Oropharyngeal Residue: Amount When Present"

table1::label(data$piriform_sinus_severity_rating) <- "Hypopharyngeal Residue"
table1::label(data$hypo_zero) <- "Hypopharyngeal Residue: Present vs. Absent"
table1::label(data$hypo_present) <- "Hypopharyngeal Residue: Amount When Present"

table1::label(data$epiglottis_severity_rating) <- "Epiglottic Residue"
table1::label(data$epi_zero) <- "Epiglottic Residue: Present vs. Absent"
table1::label(data$epi_present) <- "Epiglottic Residue: Amount When Present"

table1::label(data$laryngeal_vestibule_severity_rating) <- "Laryngeal Vestibule Residue"
table1::label(data$lv_zero) <- "Laryngeal Vestibule Residue: Present vs. Absent"
table1::label(data$lv_present) <- "Laryngeal Vestibule Residue: Amount When Present"

table1::label(data$vocal_folds_severity_rating) <- "Vocal Fold Residue"
table1::label(data$vf_zero) <- "Vocal Fold Residue: Present vs. Absent"
table1::label(data$vf_present) <- "Vocal Fold Residue: Amount When Present"

table1::label(data$subglottis_severity_rating) <- "Subglottic Residue"
table1::label(data$sg_zero) <- "Subglottic Residue: Present vs. Absent"
table1::label(data$sg_present) <- "Subglottic Residue: Amount When Present"

table1::label(data$pas_before_the_swallow) <- "PAS: Before"
table1::label(data$pas_during_the_swallow) <- "PAS: During"
table1::label(data$pas_after_the_swallow) <- "PAS: After"
table1::label(data$pas_between_bolus_trials) <- "PAS: Between"
table1::label(data$pas_max) <- "PAS"

#Create individual data sets for each swallowing condition 
task1_data <- data %>%
  filter(bolus_volume == "Self-administered cup sip (non-measured)" & bolus_consistency == "Thin (IDDSI 0)" & instruction_single_natural_uninterrupted_fast == "Natural")

task3_data <- data %>%
  filter(bolus_volume == "5 mL" & bolus_consistency == "Thin (IDDSI 0)" & instruction_single_natural_uninterrupted_fast == "Single")

task5_data <- data %>%
  filter(bolus_volume == "10 mL" & bolus_consistency == "Thin (IDDSI 0)" & instruction_single_natural_uninterrupted_fast == "Single")

task7_data <- data %>%
  filter(bolus_volume == "20 mL" & bolus_consistency == "Thin (IDDSI 0)" & instruction_single_natural_uninterrupted_fast == "Single")

task9_data <- data %>%
  filter(bolus_volume == "Self-administered cup sip (non-measured)" & bolus_consistency == "Thin (IDDSI 0)" & instruction_single_natural_uninterrupted_fast == "Single")

task11_data <- data %>%
  filter(bolus_volume == "90 mL" & bolus_colorant == "White coating" & bolus_consistency == "Thin (IDDSI 0)" & instruction_single_natural_uninterrupted_fast == "Uninterrupted")

task12_data <- data %>%
  filter(bolus_volume == "5 mL" & bolus_colorant == "Blue opaque" & bolus_consistency == "Extremely thick (IDDSI 4)" & instruction_single_natural_uninterrupted_fast == "Single")

task13_data <- data %>%
  filter(bolus_volume == "Self-administered cup sip (non-measured)" & bolus_colorant == "Natural/non-colored" & bolus_consistency == "Regular (IDDSI 7)" & instruction_single_natural_uninterrupted_fast == "Single")

task20_data <- data %>%
  filter(bolus_volume != "90 mL")

task21_data <- data %>%
  filter(bolus_volume == "Self-administered cup sip (non-measured)")

task22_data <- data %>%
  filter(bolus_volume == "Self-administered cup sip (non-measured)" & number_of_swallows_performed == "1")

task23_data <- data %>%
  filter(bolus_consistency == "Thin (IDDSI 0)" & bolus_volume != "90 mL")

task24_data <- data %>%
  filter(bolus_consistency == "Regular (IDDSI 7)" | bolus_consistency == "Extremely thick (IDDSI 4)" )


#Descriptive Statistic Tables
#Sip and bite size for self-selected volumes of water and cracker, excluding 90 mL sequential swallows 
table1::table1(~ bolus_volume_continuous_scale | 
                 instruction_single_natural_uninterrupted_fast*bolus_consistency, 
               data = task21_data,
               overall= F,
               render.continuous=c(.="Mean (SD)", 
                                   .="Median [Q1, Q3]",
                                   "Percentiles: 1%, 2.5%, 5%, 10%" = "q01, q02.5, q05, q10",
                                   "Percentiles: 90%, 95%, 97.5%, 99%" = "q90, q95, q97.5, q99",
                                   .="Min, Max"))

#Sip and bite size for self-selected volumes of water and cracker, excluding 90 mL sequential swallows 
#only for trials where patients used just one swallow during the "during the swallow" temporal boundar
table1::table1(~ bolus_volume_continuous_scale | 
                 instruction_single_natural_uninterrupted_fast*bolus_consistency, 
               data = task22_data,
               overall= F,
               render.continuous=c(.="Mean (SD)", 
                                   .="Median [Q1, Q3]",
                                   "Percentiles: 1%, 2.5%, 5%, 10%" = "q01, q02.5, q05, q10",
                                   "Percentiles: 90%, 95%, 97.5%, 99%" = "q90, q95, q97.5, q99",
                                   .="Min, Max"))

#Number of swallows across consistencies, excluding 90 mL sequential swallows
table1::table1(~ number_of_swallows_performed, 
               data = task20_data,
               overall= "Total",
               render.continuous=c(.="Mean (SD)", 
                                   .="Median [Q1, Q3]",
                                   "Percentiles: 1%, 2.5%, 5%, 10%" = "q01, q02.5, q05, q10",
                                   "Percentiles: 90%, 95%, 97.5%, 99%" = "q90, q95, q97.5, q99",
                                   .="Min, Max"))

#Descriptive statistics table for all outcome measures 
#Summarized across swallowing conditions & trials
table1::table1(~ number_of_swallows_performed +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis +
                 oro_zero +
                 oro_present +
                 hypo_zero +
                 hypo_present +
                 epi_zero +
                 epi_present +
                 lv_zero +
                 lv_present +
                 vf_zero +
                 vf_present +
                 sg_zero +
                 sg_present +
                 pas_max, 
               data = data,
               overall= "Total",
               render.continuous=c(.="Mean (SD)", 
                                   .="Median [Q1, Q3]",
                                   "Percentiles: 1%, 2.5%, 5%, 10%" = "q01, q02.5, q05, q10",
                                   "Percentiles: 90%, 95%, 97.5%, 99%" = "q90, q95, q97.5, q99",
                                   .="Min, Max"))

#Descriptive statistics table for all outcome measures 
#Summarized for the self-selected volume, thin liquid, 'natural-swallow' swallowing condition
table1::table1(~ bolus_volume_continuous_scale +
                 number_of_swallows_performed + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis +
                 oro_zero +
                 oro_present +
                 hypo_zero +
                 hypo_present +
                 epi_zero +
                 epi_present +
                 lv_zero +
                 lv_present +
                 vf_zero +
                 vf_present +
                 sg_zero +
                 sg_present +
                 pas_max | 
                 bolus_volume*bolus_colorant, 
               data = task1_data,
               overall=F,
               render.continuous=c(.="Mean (SD)", 
                                   .="Median [Q1, Q3]",
                                   "Percentiles: 1%, 2.5%, 5%, 10%" = "q01, q02.5, q05, q10",
                                   "Percentiles: 90%, 95%, 97.5%, 99%" = "q90, q95, q97.5, q99",
                                   .="Min, Max"))

#Descriptive statistics table for all outcome measures 
#Summarized for the 5 mL, thin liquid, 'single-swallow' swallowing condition
table1::table1(~ number_of_swallows_performed + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis +
                 oro_zero +
                 oro_present +
                 hypo_zero +
                 hypo_present +
                 epi_zero +
                 epi_present +
                 lv_zero +
                 lv_present +
                 vf_zero +
                 vf_present +
                 sg_zero +
                 sg_present +
                 pas_max | 
                 bolus_volume*bolus_colorant, 
               data = task3_data,
               overall=F,
               render.continuous=c(.="Mean (SD)", 
                                   .="Median [Q1, Q3]",
                                   "Percentiles: 1%, 2.5%, 5%, 10%" = "q01, q02.5, q05, q10",
                                   "Percentiles: 90%, 95%, 97.5%, 99%" = "q90, q95, q97.5, q99",
                                   .="Min, Max"))

#Descriptive statistics table for all outcome measures 
#Summarized for the 10 mL, thin liquid, 'single-swallow' swallowing condition
table1::table1(~ number_of_swallows_performed + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis +
                 oro_zero +
                 oro_present +
                 hypo_zero +
                 hypo_present +
                 epi_zero +
                 epi_present +
                 lv_zero +
                 lv_present +
                 vf_zero +
                 vf_present +
                 sg_zero +
                 sg_present +
                 pas_max | 
                 bolus_volume*bolus_colorant, 
               data = task5_data,
               overall=F,
               render.continuous=c(.="Mean (SD)", 
                                   .="Median [Q1, Q3]",
                                   "Percentiles: 1%, 2.5%, 5%, 10%" = "q01, q02.5, q05, q10",
                                   "Percentiles: 90%, 95%, 97.5%, 99%" = "q90, q95, q97.5, q99",
                                   .="Min, Max"))

#Descriptive statistics table for all outcome measures 
#Summarized for the 20 mL, thin liquid, 'single-swallow' swallowing condition
table1::table1(~ number_of_swallows_performed + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis +
                 oro_zero +
                 oro_present +
                 hypo_zero +
                 hypo_present +
                 epi_zero +
                 epi_present +
                 lv_zero +
                 lv_present +
                 vf_zero +
                 vf_present +
                 sg_zero +
                 sg_present +
                 pas_max | 
                 bolus_volume*bolus_colorant, 
               data = task7_data,
               overall=F,
               render.continuous=c(.="Mean (SD)", 
                                   .="Median [Q1, Q3]",
                                   "Percentiles: 1%, 2.5%, 5%, 10%" = "q01, q02.5, q05, q10",
                                   "Percentiles: 90%, 95%, 97.5%, 99%" = "q90, q95, q97.5, q99",
                                   .="Min, Max"))

#Descriptive statistics table for all outcome measures 
#Summarized for the self-selected volume, thin liquid, 'single-swallow' swallowing condition
table1::table1(~ bolus_volume_continuous_scale +
                 number_of_swallows_performed + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis +
                 oro_zero +
                 oro_present +
                 hypo_zero +
                 hypo_present +
                 epi_zero +
                 epi_present +
                 lv_zero +
                 lv_present +
                 vf_zero +
                 vf_present +
                 sg_zero +
                 sg_present +
                 pas_max | 
                 bolus_volume*bolus_colorant, 
               data = task9_data,
               overall=F,
               render.continuous=c(.="Mean (SD)", 
                                   .="Median [Q1, Q3]",
                                   "Percentiles: 1%, 2.5%, 5%, 10%" = "q01, q02.5, q05, q10",
                                   "Percentiles: 90%, 95%, 97.5%, 99%" = "q90, q95, q97.5, q99",
                                   .="Min, Max"))

#Descriptive statistics table for all outcome measures 
#Summarized for the 90 mL, thin liquid, 'uninterrupted/sequential swallow' swallowing condition
table1::table1(~ number_of_swallows_performed + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis +
                 oro_zero +
                 oro_present +
                 hypo_zero +
                 hypo_present +
                 epi_zero +
                 epi_present +
                 lv_zero +
                 lv_present +
                 vf_zero +
                 vf_present +
                 sg_zero +
                 sg_present +
                 pas_max | 
                 bolus_volume*bolus_colorant, 
               data = task11_data,
               overall=F,
               render.continuous=c(.="Mean (SD)", 
                                   .="Median [Q1, Q3]",
                                   "Percentiles: 1%, 2.5%, 5%, 10%" = "q01, q02.5, q05, q10",
                                   "Percentiles: 90%, 95%, 97.5%, 99%" = "q90, q95, q97.5, q99",
                                   .="Min, Max"))

#Descriptive statistics table for all outcome measures 
#Summarized for the 5 mL, pudding, 'single-swallow' swallowing condition
table1::table1(~ number_of_swallows_performed + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis +
                 oro_zero +
                 oro_present +
                 hypo_zero +
                 hypo_present +
                 epi_zero +
                 epi_present +
                 lv_zero +
                 lv_present +
                 vf_zero +
                 vf_present +
                 sg_zero +
                 sg_present +
                 pas_max | 
                 bolus_colorant, 
               data = task12_data,
               overall=F,
               render.continuous=c(.="Mean (SD)", 
                                   .="Median [Q1, Q3]",
                                   "Percentiles: 1%, 2.5%, 5%, 10%" = "q01, q02.5, q05, q10",
                                   "Percentiles: 90%, 95%, 97.5%, 99%" = "q90, q95, q97.5, q99",
                                   .="Min, Max"))

#Descriptive statistics table for all outcome measures 
#Summarized for the self-selected volume, cracker, 'single-swallow' swallowing condition
table1::table1(~ bolus_volume_continuous_scale +
                 number_of_swallows_performed + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis + 
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds +
                 bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis +
                 oro_zero +
                 oro_present +
                 hypo_zero +
                 hypo_present +
                 epi_zero +
                 epi_present +
                 lv_zero +
                 lv_present +
                 vf_zero +
                 vf_present +
                 sg_zero +
                 sg_present +
                 pas_max | 
                 bolus_colorant, 
               data = task13_data,
               overall=F,
               render.continuous=c(.="Mean (SD)", 
                                   .="Median [Q1, Q3]",
                                   "Percentiles: 1%, 2.5%, 5%, 10%" = "q01, q02.5, q05, q10",
                                   "Percentiles: 90%, 95%, 97.5%, 99%" = "q90, q95, q97.5, q99",
                                   .="Min, Max"))

# VASES Norms: Figures ------------------------------------------------
#Oropharyngeal residue figure, summarized across all swallowing conditions & trials
oro <- ggplot(data=data,aes(x=valleculae_severity_rating, fill = oro_zero, color = oro_zero)) + 
  geom_histogram(bins = 100) +
  theme_cowplot() +
  scale_x_continuous(limits = c(-1,101), 
                     breaks = seq(from = 0, to = 100, by = 5.0)) +
  scale_y_continuous(limits = c(0,200),
                     breaks = seq(from = 0, to = 500, by = 10.0)) +
  scale_fill_manual(values = c("orange", "blue"))+
  scale_color_manual(values = c("white", "white"))+
  labs(
    x = "Oropopharyngeal Residue Rating (%)",
    y = "Count") +
  easy_remove_legend()
oro

ggplot2::ggsave(
  filename = "oro.png",
  plot = last_plot(),
  width = 8.5,
  dpi = 400, 
  bg = 'white')


#Hypopharyngeal residue figure, summarized across all swallowing conditions & trials
break_trans = scales::trans_new('clip_range', 
                                \(x) ifelse(x <= 80, x, x - 70), 
                                \(x) ifelse(x <= 80, x, x + 70))

hypo <- ggplot(data=data,aes(x=piriform_sinus_severity_rating, fill = hypo_zero, color = hypo_zero)) + 
  geom_histogram(bins = 100) +
  coord_trans(y = break_trans)+
  scale_x_continuous(limits = c(-1,101), 
                     breaks = seq(from = 0, to = 100, by = 5.0)) +
  scale_y_continuous(limits = c(-1,280), 
                     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 
                                160, 170, 180, 190, 200, 210, 220,
                                230, 240, 250, 260, 270, 280)) +
  scale_fill_manual(values = c("orange", "blue"))+
  scale_color_manual(values = c("white", "white"))+
  geom_hline(yintercept=80, linetype="solid", color = "dark grey")+
  theme_cowplot() +
  labs(
    x = "Hypopopharyngeal Residue Rating (%)",
    y = "Count") +
  easy_remove_legend()
hypo

ggplot2::ggsave(
  filename = "hypo.png",
  plot = last_plot(),
  width = 8.5,
  dpi = 400, 
  bg = 'white')

#Epiglottic residue figure, summarized across all swallowing conditions & trials
epi <- ggplot(data=data,aes(x=epiglottis_severity_rating, fill = epi_zero, color = epi_zero)) + 
  geom_histogram(bins = 100) +
  theme_cowplot() +
  scale_x_continuous(limits = c(-1,102), 
                     breaks = seq(from = 0, to = 100, by = 5.0)) +
  scale_y_continuous(limits = c(-1,205),
                     breaks = seq(from = 0, to = 500, by = 10.0)) +
  scale_fill_manual(values = c("orange", "blue"))+
  scale_color_manual(values = c("white", "white"))+
  labs(
    x = "Epiglottis Residue Rating (%)",
    y = "Count") +
  easy_remove_legend()
epi

ggplot2::ggsave(
  filename = "epi.png",
  plot = last_plot(),
  width = 8.5,
  dpi = 400, 
  bg = 'white')

#Laryngeal Vesitbule residue figure, summarized across all swallowing conditions & trials
break_trans = scales::trans_new('clip_range', 
                                \(x) ifelse(x <= 50, x, x - 350), 
                                \(x) ifelse(x <= 50, x, x + 350))

lv <- ggplot(data=data,aes(x=laryngeal_vestibule_severity_rating, fill = lv_zero, color = lv_zero)) + 
  geom_histogram(bins = 105) +
  coord_trans(y = break_trans)+
  theme_cowplot() +
  scale_x_continuous(limits = c(-1,100), 
                     breaks = seq(from = 0, to = 100, by = 5.0)) +
  scale_y_continuous(limits = c(-1,450), 
                     breaks = c(0, 05, 10, 15, 20, 25, 30, 35, 40, 45, 50, 
                                405,410, 415, 420, 425, 430, 435, 440, 445, 450)) +
  geom_hline(yintercept=50, linetype="solid", color = "dark grey")+
  scale_fill_manual(values = c("orange", "blue"))+
  scale_color_manual(values = c("white", "white"))+
  labs(
    x = "Laryngeal Vestibule Residue Rating (%)",
    y = "Count") +
  easy_remove_legend()
lv

ggplot2::ggsave(
  filename = "lv.png",
  plot = last_plot(),
  width = 8.5,
  dpi = 400, 
  bg = 'white')

#Vocal fold residue figure, summarized across all swallowing conditions & trials
break_trans = scales::trans_new('clip_range', 
                                \(x) ifelse(x <= 50, x, x - 450), 
                                \(x) ifelse(x <= 50, x, x + 450))

vf <- ggplot(data=data,aes(x=vocal_folds_severity_rating, fill = vf_zero, color = vf_zero)) + 
  geom_histogram(bins = 100) +
  coord_trans(y = break_trans)+
  theme_cowplot() +
  scale_x_continuous(limits = c(-1,101), 
                     breaks = seq(from = 0, to = 100, by = 5.0)) +
  scale_y_continuous(limits = c(-1,555), 
                     breaks = c(0, 05, 10, 15, 20, 25, 30, 35, 40, 45, 50, 
                                505,510, 515, 520, 525, 530, 535, 540, 545, 550, 555)) +
  geom_hline(yintercept=50, linetype="solid", color = "dark grey")+
  scale_fill_manual(values = c("orange", "blue"))+
  scale_color_manual(values = c("white", "white"))+
  labs(
    x = "Vocal Fold Residue Rating (%)",
    y = "Count") +
  easy_remove_legend()
vf

ggplot2::ggsave(
  filename = "vf.png",
  plot = last_plot(),
  width = 8.5,
  dpi = 400, 
  bg = 'white')

#Subglottic residue figure, summarized across all swallowing conditions & trials
break_trans = scales::trans_new('clip_range', 
                                \(x) ifelse(x <= 50, x, x - 480), 
                                \(x) ifelse(x <= 50, x, x + 480))

sg <- ggplot(data=data,aes(x=subglottis_severity_rating, fill = sg_zero, color = sg_zero)) + 
  geom_histogram(bins = 100) +
  coord_trans(y = break_trans)+
  theme_cowplot() +
  scale_x_continuous(limits = c(-1,101), 
                     breaks = seq(from = 0, to = 100, by = 5.0)) +
  scale_y_continuous(limits = c(-1,580), 
                     breaks = c(0, 05, 10, 15, 20, 25, 30, 35, 40, 45, 50, 
                                535,540, 545, 550, 555, 560, 565, 570, 575, 580)) +
  scale_fill_manual(values = c("orange", "blue"))+
  scale_color_manual(values = c("white", "white"))+
  geom_hline(yintercept=50, linetype="solid", color = "dark grey")+
  labs(
    x = "Subglottic Residue Rating (%)",
    y = "Count") +
  easy_remove_legend()
sg

ggplot2::ggsave(
  filename = "sg.png",
  plot = last_plot(),
  width = 8.5,
  dpi = 400, 
  bg = 'white')

#Penetration-Aspiration Scale (PAS) figure, summarized across all swallowing conditions & trials
pas <- ggplot(data=data,aes(x=pas_max, y=..count.., fill = pas_max)) + 
  geom_bar(width=0.5) +
  theme_cowplot() +
  scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 25.0)) +
  scale_x_discrete(breaks = seq(from = 1, to = 8, by = 1.0), drop = FALSE) +
  scale_fill_manual(values=c("orange", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))+
  labs(
    x = "Penetration-Aspiration Scale (PAS)",
    y = "Count") +
  easy_remove_legend()
pas


#Bolus location at swallow onset figure for 'Oral Cavity Only', summarized across all swallowing conditions & trials
blso_oral <- ggplot(data=data,aes(x=bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset, y=..count.., fill = bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oral_cavity_no_bolus_visualized_at_onset)) + 
  geom_bar(width=0.5) +
  theme_cowplot() +
  scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 25.0)) +
  scale_fill_manual(values=c("orange", "blue", "grey"))+
  labs(
    x = "Bolus Location at Swallow Onset: Oral Cavity Only",
    y = "Count") +
  easy_remove_legend()
blso_oral

ggplot2::ggsave(
  filename = "BLSO.png",
  plot = last_plot(),
  dpi = 300, 
  bg = 'white')

#Bolus location at swallow onset figure for 'Oropharynx', summarized across all swallowing conditions & trials
blso_oro <- ggplot(data=data,aes(x=bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx, y=..count.., fill = bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_oropharynx)) + 
  geom_bar(width=0.5) +
  theme_cowplot() +
  scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 25.0)) +
  scale_fill_manual(values=c("orange", "blue", "grey"))+
  labs(
    x = "Bolus Location at Swallow Onset: Oropharynx",
    y = "Count") +
  easy_remove_legend()
blso_oro

#Bolus location at swallow onset figure for 'Hypopharynx', summarized across all swallowing conditions & trials
blso_hypo <- ggplot(data=data,aes(x=bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx, y=..count.., fill = bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_hypopharynx)) + 
  geom_bar(width=0.5) +
  theme_cowplot() +
  scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 25.0)) +
  scale_fill_manual(values=c("orange", "blue", "grey"))+
  labs(
    x = "Bolus Location at Swallow Onset: Hypopharynx",
    y = "Count") +
  easy_remove_legend()
blso_hypo

#Bolus location at swallow onset figure for 'Epiglottis', summarized across all swallowing conditions & trials
blso_epi <- ggplot(data=data,aes(x=bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis, y=..count.., fill = bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_epiglottis)) + 
  geom_bar(width=0.5) +
  theme_cowplot() +
  scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 25.0)) +
  scale_fill_manual(values=c("orange", "blue", "grey"))+
  labs(
    x = "Bolus Location at Swallow Onset: Epiglottis",
    y = "Count") +
  easy_remove_legend()
blso_epi

#Bolus location at swallow onset figure for 'Laryngeal Vestibule', summarized across all swallowing conditions & trials
blso_lv <- ggplot(data=data,aes(x=bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule, y=..count.., fill = bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_laryngeal_vestibule)) + 
  geom_bar(width=0.5) +
  theme_cowplot() +
  scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 25.0)) +
  scale_fill_manual(values=c("orange", "blue", "grey"))+
  labs(
    x = "Bolus Location at Swallow Onset: Laryngeal Vestibule",
    y = "Count") +
  easy_remove_legend()
blso_lv

#Bolus location at swallow onset figure for 'Vocal Folds', summarized across all swallowing conditions & trials
blso_vf <- ggplot(data=data,aes(x=bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds, y=..count.., fill = bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_vocal_folds)) + 
  geom_bar(width=0.5) +
  theme_cowplot() +
  scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 25.0)) +
  scale_fill_manual(values=c("orange", "blue", "grey"))+
  labs(
    x = "Bolus Location at Swallow Onset: Vocal Folds",
    y = "Count") +
  easy_remove_legend()
blso_vf

#Bolus location at swallow onset figure for 'Subglottis', summarized across all swallowing conditions & trials
blso_sg <- ggplot(data=data,aes(x=bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis, y=..count.., fill = bolus_location_at_onset_of_during_the_swallow_if_visualized_choice_subglottis)) + 
  geom_bar(width=0.5) +
  theme_cowplot() +
  scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 25.0)) +
  scale_fill_manual(values=c("blue", "grey"))+
  labs(
    x = "Bolus Location at Swallow Onset: Subglottis",
    y = "Count") +
  easy_remove_legend()
blso_sg


