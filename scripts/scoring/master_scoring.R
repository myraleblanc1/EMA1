#-------------------------------------------------------------------------------
# MASTER SCORING SCRIPT
#-------------------------------------------------------------------------------
library(tidyverse)
library(purrr)

# Load all scoring sub-functions
scoring_subscripts <- list.files("scripts/scoring/scoringsubscripts", pattern = "\\.R", full.names = TRUE)
 invisible(sapply(scoring_subscripts, source))

################################################################################
# Remove all questionnaires from the list that you do not want to score
questionnaires_to_score <- c("IRI", "PPIR40")
study_acronym <- c("STUDYACRONYM")
################################################################################


#Step 1: Sources tibbles for each Questionnaire (these hold all of the question
 #text and the question #s)
ques_tibble <- tibble()


for (ques in questionnaires_to_score) {
  tibble_path <- file.path("scripts/scoring/item_maps", paste0(ques, "_items.R"))
  
  if (file.exists(tibble_path)) {
    tibble_env <- new.env()
    source(tibble_path, local = tibble_env)  
    ques_var <- paste0(ques, "_tibble")      
    
    if (exists(ques_var, envir = tibble_env)) {
      this_tibble <- get(ques_var, envir = tibble_env) |>
        mutate(questionnaire = ques)
      
      ques_tibble <- bind_rows(ques_tibble, this_tibble)
    } else {
      warning(paste("Expected tibble", ques_var, "not found in", tibble_path))
    }
    
  } else {
    warning(paste("Tibble script for", ques, "not found. Skipping."))
  }
}


 #Step 2: Rename all of the question text to correspond with the questionnaire number
 # e.g "I.have.always.seen.myself.as.something.of.a.rebel." becomes "PPI04"
source("scripts/scoring/scoringsubscripts/scoring_rename_func.R")  
rawdata <- rename_qualfunc(file_path = "data/raw/rawdata.csv", ques_tibble) 

#Step 3: Source the scoring scripts for each questionnaire
for (ques in questionnaires_to_score) {
  script_path <- file.path("scripts/scoring/scoringsubscripts", paste0("score_", ques, ".R"))
  if (file.exists(script_path)) {
    source(script_path)
  } else {
    warning(paste("Scoring script for", ques, "not found. Skipping."))
  }}

all_scores <- list()

#Step 4: Run scoring functions
for (ques in questionnaires_to_score) {
  score_func <- paste0("score_", ques)
  if (exists(score_func)) {
    scored_data <- get(score_func)(rawdata, ques_tibble)
    all_scores[[ques]] <- scored_data
  } else {
    warning(paste("Scoring function for", ques, "not found. Skipping."))
  }
}

#Step 5: Save scores by subject number to .csv files
datestamp <- format(Sys.time(), "%Y-%m-%d-%H%M%S")

#Saves a clean .csv file that contains only the numeric scores

clean_scored_data <- list()
clean_scored_data_idx <- ncol(rawdata) + 1

for (ques in names(all_scores)) {
  df <- all_scores[[ques]]
  score_cols <- names(df)[clean_scored_data_idx:ncol(df)]
  clean_df <- df |> select(subject_id, all_of(score_cols))
  clean_scored_data[[ques]] <- clean_df
}

for (ques in names(clean_scored_data)) {
  assign(paste0(ques, "_scored_df"), clean_scored_data[[ques]])
}

clean_scored_data_all <- reduce(clean_scored_data, left_join, by = "subject_id")

write_csv(clean_scored_data_all, paste0("data/processed/scored/clean_scored_data_all_", study_acronym, datestamp, ".csv"))


for (ques in names(all_scores_clean)) {
  write_csv(all_scores[[ques]], paste0("data/processed/scored/", ques, "_scored_clean_", datestamp, ".csv"))
}





