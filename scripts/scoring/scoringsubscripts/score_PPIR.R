#PPIR-R Scoring Subscript

score_PPIR<- function(rawdata, ques_tibble) {
  library(dplyr)
  library(tibble)
  source("scripts/scoring/scoringsubscripts/scoring_rename_func.R") 
  
  # Get only PPIR items from the master item map
  iri_items <- ques_tibble |>
    filter(questionnaire == "PPIR") |>
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
  reversed_items <- c("PPIR05", "PPIR07", "PPIR08", "PPIR10", "PPIR13", "PPIR15", "PPIR16", "PPIR18", "PPIR20", 
                      "PPIR21", "PPIR23", "PPIR24", "PPIR29", "PPIR31", "PPIR32", "PPIR34", "PPIR37", "PPIR40", 
                      "PPIR42", "PPIR45", "PPIR48", "PPIR50", "PPIR53", "PPIR54", "PPIR55"
  )
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
     
      machiavellian_egocentricity = rowSums(across(all_of(c(
        "PPIR01","PPIR23","PPIR45","PPIR67","PPIR11","PPIR33","PPIR55","PPIR77",
        "PPIR17","PPIR39","PPIR61","PPIR83","PPIR92","PPIR114","PPIR136",
        "PPIR103","PPIR125","PPIR147","PPIR132","PPIR154"
      ))), na.rm = TRUE),
      
      rebellious_nonconformity = rowSums(across(all_of(c(
        "PPIR04","PPIR26","PPIR48","PPIR70","PPIR14","PPIR15","PPIR36","PPIR58",
        "PPIR80","PPIR94","PPIR116","PPIR138","PPIR104","PPIR105","PPIR127","PPIR149"
      ))), na.rm = TRUE),
      
      blame_externalization = rowSums(across(all_of(c(
        "PPIR16","PPIR38","PPIR60","PPIR82","PPIR18","PPIR19","PPIR40","PPIR62",
        "PPIR84","PPIR90","PPIR112","PPIR134","PPIR100","PPIR122","PPIR144"
      ))), na.rm = TRUE),
      
      carefree_nonplanfulness = rowSums(across(all_of(c(
        "PPIR07","PPIR29","PPIR51","PPIR73","PPIR44","PPIR66","PPIR88","PPIR89",
        "PPIR111","PPIR133","PPIR99","PPIR121","PPIR143","PPIR101","PPIR123","PPIR145",
        "PPIR108","PPIR130","PPIR152"
      ))), na.rm = TRUE),
      
      social_influence = rowSums(across(all_of(c(
        "PPIR02","PPIR24","PPIR46","PPIR68","PPIR34","PPIR56","PPIR78","PPIR41",
        "PPIR63","PPIR85","PPIR21","PPIR22","PPIR43","PPIR65","PPIR87","PPIR91",
        "PPIR113","PPIR135"
      ))), na.rm = TRUE),
      
      fearlessness = rowSums(across(all_of(c(
        "PPIR03","PPIR25","PPIR47","PPIR69","PPIR12","PPIR13","PPIR35","PPIR57",
        "PPIR79","PPIR93","PPIR115","PPIR137","PPIR126","PPIR148"
      ))), na.rm = TRUE),
      
      stress_immunity = rowSums(across(all_of(c(
        "PPIR06","PPIR28","PPIR50","PPIR72","PPIR10","PPIR32","PPIR54","PPIR76",
        "PPIR96","PPIR118","PPIR119","PPIR140","PPIR141"
      ))), na.rm = TRUE),
      
      coldheartedness = rowSums(across(all_of(c(
        "PPIR05","PPIR27","PPIR49","PPIR71","PPIR09","PPIR31","PPIR53","PPIR75",
        "PPIR97","PPIR98","PPIR120","PPIR142","PPIR109","PPIR110","PPIR131","PPIR153"
      ))), na.rm = TRUE),
      
     
      virtuous_responding = rowSums(across(all_of(c(
        "PPIR37","PPIR59","PPIR81","PPIR20","PPIR42","PPIR64","PPIR86","PPIR95",
        "PPIR117","PPIR139","PPIR106","PPIR128","PPIR150"
      ))), na.rm = TRUE),
      
      deviant_responding = rowSums(across(all_of(c(
        "PPIR08","PPIR30","PPIR52","PPIR74","PPIR102","PPIR124","PPIR146",
        "PPIR107","PPIR129","PPIR151"
      ))), na.rm = TRUE),
   
      PPIR_Total = rowSums(across(starts_with("PPIR")), na.rm = TRUE),
 
      self_centered_impulsivity = machiavellian_egocentricity + rebellious_nonconformity +
        blame_externalization + carefree_nonplanfulness,
      
      fearless_dominance = social_influence + fearlessness + stress_immunity
      

    ) |>
    select(subject_id, everything())
                                                                                    
  
  return(scored)
}
