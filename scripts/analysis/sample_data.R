#CREATE SAMPLE DATA FOR TESTING
library(tidyverse)
library(here)
#-------------------------------------------------------------------------------
#CREATE SINGLE FILE FOR TESTING

set.seed(123)

n <- 30

sample_data <- tibble(
  participant_id = paste0("P", sprintf("%03d", 1:n)),
  group = sample(c("control", "treatment"), n, replace = TRUE),
  age = sample(18:35, n, replace = TRUE),
  gender = sample(c("Male", "Female", "Other"), n, replace = TRUE, prob = c(0.45, 0.45, 0.1)),
  BDI_1 = sample(0:3, n, replace = TRUE),
  BDI_2 = sample(0:3, n, replace = TRUE),
  BDI_3 = sample(0:3, n, replace = TRUE),
  BDI_4 = sample(0:3, n, replace = TRUE),
  BDI_5 = sample(0:3, n, replace = TRUE),
  reaction_time = rnorm(n, mean = 500, sd = 80)
)

# Introduce a few missing values
sample_data$BDI_3[sample(1:n, 2)] <- NA
sample_data$reaction_time[sample(1:n, 1)] <- NA

# Save to raw data folder
if (!dir.exists("data/raw")) {
  dir.create("data/raw", recursive = TRUE)
}

write_csv(sample_data, "data/raw/sample_data.csv")

#------------------------------------------------------------------------------
#CREATE SAMPLE DATA SETS FOR SEPERATE DATA FILES
# -----------------------------
# Set Study Acronym
# -----------------------------
study_acronym <- "STUDYACRONYM"  # e.g., "EMA1"

# -----------------------------
# Generate Subject IDs
# -----------------------------
set.seed(123)  # for reproducibility
subject_ids <- paste0("S", str_pad(1:100, width = 3, pad = "0"))

# -----------------------------
# Generate Behavioral Data
# -----------------------------
behavioral_data <- tibble(
  SUBID = subject_ids,
  reaction_time_ms = round(rnorm(100, mean = 600, sd = 100)),  # realistic RT
  accuracy = rbinom(100, size = 1, prob = 0.85)                # mostly correct
)

# -----------------------------
# Generate Imaging Data
# -----------------------------
imaging_data <- tibble(
  subject_id = subject_ids,
  FA_left_PFC  = round(rnorm(100, mean = 0.4, sd = 0.05), 3),
  MD_right_PFC = round(rnorm(100, mean = 0.8, sd = 0.04), 3)
)

# -----------------------------
# Generate Survey Data
# -----------------------------
survey_data <- tibble(
  subject_id = subject_ids,
  age = sample(18:35, 100, replace = TRUE),
  gender = sample(c("female", "male", "nonbinary", "prefer_not_to_say"), 100, replace = TRUE, prob = c(0.45, 0.45, 0.05, 0.05)),
  BDI_score = round(rnorm(100, mean = 10, sd = 6))  # Beck Depression Inventory
)

# -----------------------------
# Save Data to /data/raw/
# -----------------------------

# Save CSV files
write_csv(behavioral_data, here("data", "raw", paste0("BEHAVIOURAL_data.csv")))
write_csv(imaging_data,    here("data", "raw", paste0("IMAGING_data.csv")))
write_csv(survey_data,     here("data", "raw", paste0("SURVEY_data.csv")))

