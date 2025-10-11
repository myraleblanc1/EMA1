#BDI-II Scoring Subscript
#DO NOT EDIT####################################################################

score_BDI<- function(rawdata) {
  library(dplyr)
  library(tibble)
  source("scripts/scoring/scoringsubscripts/scoring_rename_func.R") 
  
  # Create PPIR-40 item mapping
  BDI_tibble <- tibble(
    text = c(
      "Sadness",
      "Pessimism",
      "Failure",
      "Loss of Pleasure",
      "Guilty Feelings",
      "Punishment Feelings",
      "Self Dislike",
      "Self Criticism",
      #"Suicidal Thoughts and Dying", This question is skipped because of REB recommendations
      "Crying",
      "Agitation",
      "Loss of Interest",
      "Indecisiveness",
      "Worthlessness",
      "Loss of Energy",
      "Change in Sleeping Pattern",
      "Irritability",
      "Changes in Appetite",
      "Concentration Difficulty",
      "Tiredness or Fatigue",
      "Loss of Interest in Sex"
    ),
    item = bdi_items <- c(
      "BDI01", "BDI02", "BDI03", "BDI04", "BDI05", "BDI06", "BDI07",
      "BDI08",           # BDI09 was the suicidality item, see above
      "BDI10", "BDI11", "BDI12", "BDI13", "BDI14",
      "BDI15", "BDI16", "BDI17", "BDI18", "BDI19", "BDI20", "BDI21"
    )
    
    
  )
  
  ques_tibble <- BDI_tibble
  
  rawdata <- rename_qualfunc(file_path = "data/raw/rawdata.csv", ques_tibble)
  
  # Recode response text (including logical TRUE/FALSE) to numeric (1–4)
  recoded <- rawdata |> 
    mutate(
      BDI01 = case_when(
        str_detect(BDI01, "do not feel sad") ~ 0L,
        str_detect(BDI01, "feel sad much of the time") ~ 1L,
        str_detect(BDI01, "sad all the time") ~ 2L,
        str_detect(BDI01, "so sad or unhappy") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI02 = case_when(
        str_detect(BDI02, "not discouraged") ~ 0L,
        str_detect(BDI02, "feel discouraged") ~ 1L,
        str_detect(BDI02, "do not expect things to work out") ~ 2L,
        str_detect(BDI02, "future is hopeless") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI03 = case_when(
        str_detect(BDI03, "do not feel like a failure") ~ 0L,
        str_detect(BDI03, "failed more than I should") ~ 1L,
        str_detect(BDI03, "see a lot of failures") ~ 2L,
        str_detect(BDI03, "total failure") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI04 = case_when(
        str_detect(BDI04, "get as much pleasure as I ever did") ~ 0L,
        str_detect(BDI04, "don't enjoy things as much") ~ 1L,
        str_detect(BDI04, "get very little pleasure") ~ 2L,
        str_detect(BDI04, "can't get any pleasure") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI05 = case_when(
        str_detect(BDI05, "don't feel particularly guilty") ~ 0L,
        str_detect(BDI05, "feel quite guilty most of the time") ~ 1L,
        str_detect(BDI05, "feel guilty over many things") ~ 2L,
        str_detect(BDI05, "feel guilty all of the time") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI06 = case_when(
        str_detect(BDI06, "don't feel I am being punished") ~ 0L,
        str_detect(BDI06, "feel I may be punished") ~ 1L,
        str_detect(BDI06, "feel I am being punished") ~ 2L,
        str_detect(BDI06, "expect to be punished") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI07 = case_when(
        str_detect(BDI07, "feel the same about myself as ever") ~ 0L,
        str_detect(BDI07, "disappointed in myself") ~ 1L,
        str_detect(BDI07, "lost confidence in myself") ~ 2L,
        str_detect(BDI07, "dislike myself") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI08 = case_when(
        str_detect(BDI08, "don't criticize or blame myself more than usual") ~ 0L,
        str_detect(BDI08, "criticize myself for all my faults") ~ 1L,
        str_detect(BDI08, "blame myself for everything bad that happens") ~ 2L,
        str_detect(BDI08, "more critical of myself than I used to be") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI10 = case_when(
        str_detect(BDI10, "don't cry any more than I used to") ~ 0L,
        str_detect(BDI10, "feel like crying but I can't") ~ 1L,
        str_detect(BDI10, "cry more than I used to") ~ 2L,
        str_detect(BDI10, "cry over every little thing") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI11 = case_when(
        str_detect(BDI11, "no more restless or wound up than usual") ~ 0L,
        str_detect(BDI11, "feel more restless or wound up than usual") ~ 1L,
        str_detect(BDI11, "so restless or agitated it's hard to stay still") ~ 2L,
        str_detect(BDI11, "so restless or agitated I have to keep moving") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI12 = case_when(
        str_detect(BDI12, "have not lost interest in other people or activities") ~ 0L,
        str_detect(BDI12, "less interested in other people or things") ~ 1L,
        str_detect(BDI12, "lost most of my interest in other people or things") ~ 2L,
        str_detect(BDI12, "hard to get interested in anything") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI13 = case_when(
        str_detect(BDI13, "make decisions about as well as ever") ~ 0L,
        str_detect(BDI13, "find it more difficult to make decisions") ~ 1L,
        str_detect(BDI13, "trouble making any decisions") ~ 2L,
        str_detect(BDI13, "much greater difficulty in making decisions") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI14 = case_when(
        str_detect(BDI14, "do not feel I am worthless") ~ 0L,
        str_detect(BDI14, "feel more worthless compared to other people") ~ 1L,
        str_detect(BDI14, "don't consider myself as worthwhile or useful") ~ 2L,
        str_detect(BDI14, "feel utterly worthless") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI15 = case_when(
        str_detect(BDI15, "have as much energy as ever") ~ 0L,
        str_detect(BDI15, "don't have enough energy to do very much") ~ 1L,
        str_detect(BDI15, "less energy than I used to have") ~ 2L,
        str_detect(BDI15, "don't have enough energy to do anything") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI16 = case_when(
        str_detect(BDI16, "have not experienced any change in my sleeping pattern") ~ 0L,
        str_detect(BDI16, "sleep somewhat less than usual") ~ 1L,
        str_detect(BDI16, "sleep a lot less than usual") ~ 2L,
        str_detect(BDI16, "sleep most of the day") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI17 = case_when(
        str_detect(BDI17, "no more irritable than usual") ~ 0L,
        str_detect(BDI17, "more irritable than usual") ~ 1L,
        str_detect(BDI17, "irritable all the time") ~ 2L,
        str_detect(BDI17, "much more irritable than usual") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI18 = case_when(
        str_detect(BDI18, "no appetite at all") ~ 0L,
        str_detect(BDI18, "appetite is somewhat less than usual") ~ 1L,
        str_detect(BDI18, "appetite is somewhat greater than usual") ~ 2L,
        str_detect(BDI18, "crave food all the time") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI19 = case_when(
        str_detect(BDI19, "can concentrate as well as ever") ~ 0L,
        str_detect(BDI19, "can't concentrate as well as usual") ~ 1L,
        str_detect(BDI19, "hard to keep my mind on anything") ~ 2L,
        str_detect(BDI19, "can't concentrate on anything") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI20 = case_when(
        str_detect(BDI20, "no more tired or fatigued than usual") ~ 0L,
        str_detect(BDI20, "get more tired or fatigued more easily") ~ 1L,
        str_detect(BDI20, "too tired or fatigued to do a lot") ~ 2L,
        str_detect(BDI20, "too tired or fatigued to do most") ~ 3L,
        TRUE ~ NA_integer_
      ),
      BDI21 = case_when(
        str_detect(BDI21, "have not noticed any recent changes in my interest in sex") ~ 0L,
        str_detect(BDI21, "less interested in sex than I used to be") ~ 1L,
        str_detect(BDI21, "much less interested in sex now") ~ 2L,
        str_detect(BDI21, "lost interest in sex completely") ~ 3L,
        TRUE ~ NA_integer_
      )
    )
  
  
  # Score subscales
  scored <- recoded |>
    mutate(
      BDI_Total = rowSums(across(all_of(c("BDI01", "BDI02", "BDI03", "BDI04", "BDI05", "BDI06", "BDI07",
                                          "BDI08",           # BDI09 was the suicidality item, see above
                                          "BDI10", "BDI11", "BDI12", "BDI13", "BDI14",
                                          "BDI15", "BDI16", "BDI17", "BDI18", "BDI19", "BDI20", "BDI21"
      ))), na.rm = TRUE),
    ) |>
    select(subject_id, everything())
  
  return(scored)
}
