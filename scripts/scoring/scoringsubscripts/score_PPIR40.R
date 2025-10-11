#SCORES PPIR_40
#DO NOT EDIT####################################################################

score_PPIR40 <- function(rawdata, ques_tibble) {
  library(dplyr)
  library(tibble)
  
  # Get only PPIR40 items from the master item map
  ppir40_items <- ques_tibble |>
    filter(questionnaire == "PPIR40") |>
    pull(item)
  
  recoded <- rawdata |>
    mutate(across(
      any_of(ppir40_items),
      ~ case_when(
        trimws(tolower(as.character(.))) %in% c("true") ~ 1L,
        trimws(as.character(.)) == "Mostly True"        ~ 2L,
        trimws(as.character(.)) == "Mostly False"       ~ 3L,
        trimws(tolower(as.character(.))) %in% c("false") ~ 4L,
        TRUE ~ NA_integer_
      )
    ))
  
  # Reverse-score
  reversed_items <- c("PPI10", "PPI22", "PPI27", "PPI47", "PPI75", "PPI76", 
                      "PPI87", "PPI89", "PPI97", "PPI108", "PPI109", "PPI113", 
                      "PPI119", "PPI121", "PPI130", "PPI145", "PPI153")
  valid_reversed <- reversed_items[reversed_items %in% colnames(recoded)]
  
  recoded <- recoded |>
    mutate(across(
      all_of(valid_reversed),
      ~ dplyr::recode(
        as.character(.),
        "1" = 4L,
        "2" = 3L,
        "3" = 2L,
        "4" = 1L,
        .default = NA_integer_
      )
    ))
  
  # Score subscales
  scored <- recoded |>
    mutate(
      Blame_externalization = rowSums(across(all_of(c("PPI18", "PPI19", "PPI40", "PPI84", "PPI122") %>% intersect(colnames(recoded)))), na.rm = TRUE),
      Carefree_nonplanfulness = rowSums(across(all_of(c("PPI89", "PPI108", "PPI121", "PPI130", "PPI145") %>% intersect(colnames(recoded)))), na.rm = TRUE),
      Coldheartedness = rowSums(across(all_of(c("PPI27", "PPI75", "PPI97", "PPI109", "PPI153") %>% intersect(colnames(recoded)))), na.rm = TRUE),
      Fearlessness = rowSums(across(all_of(c("PPI12", "PPI47", "PPI115", "PPI137", "PPI148") %>% intersect(colnames(recoded)))), na.rm = TRUE),
      Machiavellian_egocentricity = rowSums(across(all_of(c("PPI33", "PPI67", "PPI77", "PPI136", "PPI154") %>% intersect(colnames(recoded)))), na.rm = TRUE),
      Rebellious_nonconformity = rowSums(across(all_of(c("PPI04", "PPI36", "PPI58", "PPI80", "PPI149") %>% intersect(colnames(recoded)))), na.rm = TRUE),
      Social_influence = rowSums(across(all_of(c("PPI22", "PPI34", "PPI46", "PPI87", "PPI113") %>% intersect(colnames(recoded)))), na.rm = TRUE),
      Stress_immunity = rowSums(across(all_of(c("PPI10", "PPI32", "PPI76", "PPI119", "PPI140") %>% intersect(colnames(recoded)))), na.rm = TRUE)
    ) |>
    mutate(
      SCI = rowSums(across(c(Machiavellian_egocentricity, Rebellious_nonconformity, Blame_externalization, Carefree_nonplanfulness)), na.rm = TRUE),
      FD = rowSums(across(c(Social_influence, Fearlessness, Stress_immunity)), na.rm = TRUE),
      PPI_Total = rowSums(across(c(SCI, FD, Coldheartedness)), na.rm = TRUE)
    ) |>
    select(subject_id, everything())
  
  return(scored)
}
