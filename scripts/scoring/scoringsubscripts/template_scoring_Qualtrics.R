#NEW QUALTRICS STUDY SCQUESTIONNAIRENG TEMPLATE
#QUESTIONNAIRE Scoring Subscript
#DO NOT EDIT####################################################################

score_QUESTIONNAIRE<- function(rawdata) {
  library(dplyr)
  library(tibble)
  source("scripts/scoring/scoringsubscripts/scoring_rename_func.R") 
  
  # Create QUESTIONNAIRE item mapping
  #Change the text and item to be relative to the new Questionnaire you want to score
  #Text should include exactly the same text as in the Qualtrics survey which is outputted in row3
  #Item should be the Questionnaire aac
  QUESTIONNAIRE_tibble <- tibble(
    text = c(
      "I daydream and fantasize, with some regularity, about things that might happen to me.",	
      "I often have tender, concerned feelings for people less fortunate than me.",
      "I sometimes find it difficult to see things from the \"other guy's\" point of view.",
      "Sometimes I don't feel very sorry for other people when they are having problems.",
      "I really get involved with the feelings of the characters in a novel.",
      "In emergency situations, I feel apprehensive and ill-at-ease.",
      "I am usually objective when I watch a movie or play, and I don't often get completely caught up in it.",
      "I try to look at everybody's side of a disagreement before I make a decision.",
      "When I see someone being taken advantage of, I feel kind of protective towards them.",
      "I sometimes feel helpless when I am in the middle of a very emotional situation.",
      "I sometimes try to understand my friends better by imagining how things look from their perspective.",
      "Becoming extremely involved in a good book or movie is somewhat rare for me.",
      "When I see someone get hurt, I tend to remain calm.",
      "Other people's misfortunes do not usually disturb me a great deal.",
      "If I'm sure I'm right about something, I don't waste much time listening to other people's arguments.",
      "After seeing a play or movie, I have felt as though I were one of the characters.",
      "Being in a tense emotional situation scares me.",
      "When I see someone being treated unfairly, I sometimes don't feel very much pity for them.",
      "I am usually pretty effective in dealing with emergencies.",
      "I am often quite touched by things that I see happen.",
      "I believe that there are two sides to every question and try to look at them both.",
      "I would describe myself as a pretty soft-hearted person.",
      "When I watch a good movie, I can very easily put myself in the place of a leading character.",
      "I tend to lose control during emergencies.",
      "When I'm upset at someone, I usually try to \"put myself in his shoes\" for a while.",
      "When I am reading an interesting story or novel, I imagine how I would feel if the events in the story were happening to me.",
      "When I see someone who badly needs help in an emergency, I go to pieces.",
      "Before criticizing somebody, I try to imagine how I would feel if I were in their place."
    ),
    item = c("QUESTIONNAIRE01", "QUESTIONNAIRE02", "QUESTIONNAIRE03", "QUESTIONNAIRE04", "QUESTIONNAIRE05", "QUESTIONNAIRE06", "QUESTIONNAIRE07", "QUESTIONNAIRE08", "QUESTIONNAIRE09", "QUESTIONNAIRE10", 
             "QUESTIONNAIRE11", "QUESTIONNAIRE12", "QUESTIONNAIRE13", "QUESTIONNAIRE14", "QUESTIONNAIRE15", "QUESTIONNAIRE16", "QUESTIONNAIRE17", "QUESTIONNAIRE18", "QUESTIONNAIRE19", "QUESTIONNAIRE20", 
             "QUESTIONNAIRE21", "QUESTIONNAIRE22", "QUESTIONNAIRE23", "QUESTIONNAIRE24", "QUESTIONNAIRE25", "QUESTIONNAIRE26", "QUESTIONNAIRE27", "QUESTIONNAIRE28")
    
  )
  
  ques_tibble <- QUESTIONNAIRE_tibble
  
  rawdata <- rename_qualfunc(file_path = "data/raw/rawdata.csv", ques_tibble)
  rawdata <- rawdata[, !is.na(colnames(rawdata)) & colnames(rawdata) != ""]
  
  
  # Recode response text (including logical TRUE/FALSE) to numeric (1–4)
  
  valid_items <- ques_tibble$item[!is.na(ques_tibble$item) & ques_tibble$item != ""]
  valid_items <- valid_items[valid_items %in% colnames(rawdata)]
  
  
  recoded <- rawdata |>
    mutate(across(
      all_of(valid_items),
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
  reversed_items <- c("QUESTIONNAIRE04", "QUESTIONNAIRE07", "QUESTIONNAIRE08", "QUESTIONNAIRE10", "QUESTIONNAIRE11", "QUESTIONNAIRE12", "QUESTIONNAIRE14", "QUESTIONNAIRE15", "QUESTIONNAIRE17", 
                      "QUESTIONNAIRE18", "QUESTIONNAIRE19", "QUESTIONNAIRE21", "QUESTIONNAIRE23", "QUESTIONNAIRE25", "QUESTIONNAIRE26", "QUESTIONNAIRE27", "QUESTIONNAIRE28")
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
     SUBSCALE1 = rowSums(across(all_of(c("QUESTIONNAIRE03", "QUESTIONNAIRE08", "QUESTIONNAIRE11", "QUESTIONNAIRE15", "QUESTIONNAIRE21", "QUESTIONNAIRE25", "QUESTIONNAIRE28"))), na.rm = TRUE),
      
      SUBSCALE2 = rowSums(across(all_of(c(
        "QUESTIONNAIRE01", "QUESTIONNAIRE05", "QUESTIONNAIRE07", "QUESTIONNAIRE12", "QUESTIONNAIRE16", "QUESTIONNAIRE23", "QUESTIONNAIRE26"))), na.rm = TRUE),
      
      SUBSCALE3 = rowSums(across(all_of(c("QUESTIONNAIRE02", "QUESTIONNAIRE04", "QUESTIONNAIRE09", "QUESTIONNAIRE14", "QUESTIONNAIRE18", "QUESTIONNAIRE20", "QUESTIONNAIRE22"))), na.rm = TRUE),
      
      SUBSCALE4 = rowSums(across(all_of(c("QUESTIONNAIRE06", "QUESTIONNAIRE10", "QUESTIONNAIRE13", "QUESTIONNAIRE17", "QUESTIONNAIRE19", "QUESTIONNAIRE24", "QUESTIONNAIRE27"))), na.rm = TRUE)
    ) |>
    mutate(
      QUESTIONNAIRE_Total = rowSums(across(all_of(c("QUESTIONNAIRE04", "QUESTIONNAIRE07", "QUESTIONNAIRE08", "QUESTIONNAIRE10", "QUESTIONNAIRE11", "QUESTIONNAIRE12", "QUESTIONNAIRE14", "QUESTIONNAIRE15",
                                          "QUESTIONNAIRE17", "QUESTIONNAIRE18", "QUESTIONNAIRE19", "QUESTIONNAIRE21", "QUESTIONNAIRE23", "QUESTIONNAIRE25", "QUESTIONNAIRE26", "QUESTIONNAIRE27", "QUESTIONNAIRE28"))), na.rm = TRUE)
    ) |>
    select(subject_id, everything())
  
  
  return(scored)
}
