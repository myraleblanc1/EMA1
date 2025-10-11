#IRI Scoring Subscript
#DO NOT EDIT####################################################################

score_IRI<- function(rawdata, ques_tibble) {
  library(dplyr)
  library(tibble)
  source("scripts/scoring/scoringsubscripts/scoring_rename_func.R") 
  
  # Get only IRI items from the master item map
  iri_items <- ques_tibble |>
    filter(questionnaire == "IRI") |>
    pull(item)

  recoded <- rawdata |>
    mutate(across(
      all_of(iri_items),
      ~ case_when(
        trimws(tolower(as.character(.))) %in% c("a (does not describe me well)") ~ 0L,
        trimws(tolower(as.character(.))) %in% c("b") ~ 1L,
        trimws(tolower(as.character(.))) %in% c("c")~ 2L,
        trimws(tolower(as.character(.))) %in% c("d") ~ 3L,
        trimws(tolower(as.character(.))) %in% c("e (describes me well)") ~ 4L,
        TRUE ~ NA_integer_
      )
    ))
  
  
  
  # Reverse-score items (1 <-> 4, 2 <-> 3)
  reversed_items <- c("IRI04", "IRI07", "IRI08", "IRI10", "IRI11", "IRI12", "IRI14", "IRI15", "IRI17", 
                      "IRI18", "IRI19", "IRI21", "IRI23", "IRI25", "IRI26", "IRI27", "IRI28")
  valid_reversed <- reversed_items[reversed_items %in% colnames(recoded)]
  
  
  recoded <- recoded |>
    mutate(across(
      all_of(valid_reversed),
      ~ dplyr::recode(
        as.character(.),
        "0" = 4L,
        "1" = 3L,
        "2" = 2L,
        "3" = 1L,
        "4" = 0L,
        .default = NA_integer_
      )
    ))
  
  # Score subscales

  
scored <- recoded |>
  mutate(
    perspective_taking = rowSums(across(all_of(c("IRI03", "IRI08", "IRI11", "IRI15", "IRI21", "IRI25", "IRI28"))), na.rm = TRUE),
    
    fantasy = rowSums(across(all_of(c(
      "IRI01", "IRI05", "IRI07", "IRI12", "IRI16", "IRI23", "IRI26"))), na.rm = TRUE),
    
    empathic_concern = rowSums(across(all_of(c("IRI02", "IRI04", "IRI09", "IRI14", "IRI18", "IRI20", "IRI22"))), na.rm = TRUE),
    
    personal_distress = rowSums(across(all_of(c("IRI06", "IRI10", "IRI13", "IRI17", "IRI19", "IRI24", "IRI27"))), na.rm = TRUE)
    ) |>
   mutate(
    IRI_Total = rowSums(across(all_of(c("IRI01", "IRI02", "IRI03", "IRI04", "IRI05", "IRI06", "IRI07", "IRI08", "IRI09", "IRI10", 
                                        "IRI11", "IRI12", "IRI13", "IRI14", "IRI15", "IRI16", "IRI17", "IRI18", "IRI19", "IRI20", 
                                        "IRI21", "IRI22", "IRI23", "IRI24", "IRI25", "IRI26", "IRI27", "IRI28"))), na.rm = TRUE)
        ) |>
          select(subject_id, everything())
                                                                                    
  
  return(scored)
}
