#IMPORT THE DATA AND CLEAN IT FOR ANALYSIS
library(tidyverse)
library(dplyr)

#-------------------------------------------------------------------------------
#IMPORTING ALL DATA

#import eprime
posterData <- read_delim(
  'data/posterData.txt',   # The name of your *.txt file output from Emerge
  delim = '\t', # These data are tab separated
) 

#import eprime 09 29 (roughly 100 participants)
posterData <- read_delim(
  'data/data_spss.txt',   # The name of your *.txt file output from Emerge
  delim = '\t', # These data are tab separated
) 



#Clean E-prime Data
posterData_clean <- 
  posterData %>%                  
  select(
    Subject = Subject,           # subject
    A1Block = A1Block,          
    A2Block = A2Block,          
    B1Block = B1Block,          
    B2Block = B2Block,          
    KResp = KResp,              
    NResp = NResp,              
    Weight1 = Weight1,          
    Weight2 = Weight2,          
    NegIacc = PracResp1.ACC,    # Negative Image
    NegIrt = PracResp1.RT,      
    NegSacc = PracResp2.ACC,    # Negative No Image
    NegSrt = PracResp2.RT,      
    NeuIacc = PracResp11.ACC,   # Neutral Image
    NeuIrt = PracResp11.RT,     
    NeuSacc = PracResp12.ACC,   # Neutral No Image
    NeuSrt = PracResp12.RT,     
    PosIacc = PracResp9.ACC,    # Positive Image
    PosIrt = PracResp9.RT,      
    PosSacc = PracResp10.ACC,   # Positive No Image
    PosSrt = PracResp10.RT,     
    FixDur = FixDuration,
    ITIDur = ITIDuration,
  )

#import Qualtrics
Qualtrics <- read.csv("data/Qualtrics0929.csv")

#Clean Qualtrics
Qualtrics_clean <- 
  Qualtrics %>%                  
  select(
    Subject = Q247,
    Age = Q2,
    Ethinicity = Q3,
    Gender = Q1,
    PPIR40 = SC1,
  )

#import eprime excel w/ conds
EprimeFiles <- read.csv("data/EMA1_eprime_files.csv")

#Clean EprimeFiles
EprimeFiles_clean <- 
  EprimeFiles %>%                  
  select(
    Subject = Subject,
    ControlCond = Control.Condition,
    RewardCond = Reward.Condition,
  )

#-------------------------------------------------------------------------------

#make all Subjects the same data type 
posterData_clean$Subject <- as.character(posterData_clean$Subject)
Qualtrics_clean$Subject <- as.character(Qualtrics_clean$Subject)
EprimeFiles_clean$Subject <- as.character(EprimeFiles$Subject)


Qualtrics_clean <- Qualtrics_clean %>%
  distinct(Subject, .keep_all = TRUE)

EprimeFiles_clean <- EprimeFiles_clean %>%
  distinct(Subject, .keep_all = TRUE)

#merge datasets by subject
joinedData <- posterData_clean %>%
  inner_join(Qualtrics_clean, by = "Subject") %>%
  inner_join(EprimeFiles_clean, by = "Subject")

#-------------------------------------------------------------------------------
#Fix all datatypes for final dataset
#Factor
EMA_factors <- joinedData %>%
  mutate(across(
    .cols = c(A1Block, A2Block, B1Block, B2Block, 
              NegIacc, NegSacc, NeuIacc, NeuSacc, 
              PosIacc, PosSacc, Weight1, Weight2, 
              Subject, Ethinicity, Gender, ControlCond,
              RewardCond, KResp, NResp),
    .fns = factor
  ))

#number
EMA <- EMA_factors %>%
  mutate(across(
    .cols = c(Age, PPIR40),
    .fns = as.numeric
  ))

#-------------------------------------------------------------------------------
# Create summary datasets and save them

# Create subject demographic data
subject_data <- EMA %>%
  select(Subject, Age, Gender, Ethinicity, PPIR40) %>%
  distinct(Subject, .keep_all = TRUE)

# Create dataset with only accurate trials
accEMA <- EMA %>%
  filter(
    (NegIacc == 1 | is.na(NegIacc)),
    (NegSacc == 1 | is.na(NegSacc)),
    (NeuIacc == 1 | is.na(NeuIacc)),
    (NeuSacc == 1 | is.na(NeuSacc)),
    (PosIacc == 1 | is.na(PosIacc)),
    (PosSacc == 1 | is.na(PosSacc))
  )

# Calculate mean RTs for each specific condition
condition_rt_means <- EMA %>%
  group_by(Subject, RewardCond, ControlCond) %>%
  summarise(
    # Mean RT for each condition
    NegI_meanRT = mean(NegIrt, na.rm = TRUE),
    NegS_meanRT = mean(NegSrt, na.rm = TRUE),
    NeuI_meanRT = mean(NeuIrt, na.rm = TRUE),
    NeuS_meanRT = mean(NeuSrt, na.rm = TRUE),
    PosI_meanRT = mean(PosIrt, na.rm = TRUE),
    PosS_meanRT = mean(PosSrt, na.rm = TRUE),
    
    # Combined means
    Neg_meanRT = mean(c(NegIrt, NegSrt), na.rm = TRUE),
    Neu_meanRT = mean(c(NeuIrt, NeuSrt), na.rm = TRUE),
    Pos_meanRT = mean(c(PosIrt, PosSrt), na.rm = TRUE),
    All_meanRT = mean(c(NegIrt, NegSrt, NeuIrt, NeuSrt, PosIrt, PosSrt), na.rm = TRUE),
    
    .groups = 'drop'
  )

# Calculate mean RTs for accurate trials only
acc_condition_rt_means <- accEMA %>%
  group_by(Subject, RewardCond, ControlCond) %>%
  summarise(
    # Mean RT for each condition (accurate trials only)
    Acc_NegI_meanRT = mean(NegIrt, na.rm = TRUE),
    Acc_NegS_meanRT = mean(NegSrt, na.rm = TRUE),
    Acc_NeuI_meanRT = mean(NeuIrt, na.rm = TRUE),
    Acc_NeuS_meanRT = mean(NeuSrt, na.rm = TRUE),
    Acc_PosI_meanRT = mean(PosIrt, na.rm = TRUE),
    Acc_PosS_meanRT = mean(PosSrt, na.rm = TRUE),
    
    # Combined means (accurate trials only)
    Acc_Neg_meanRT = mean(c(NegIrt, NegSrt), na.rm = TRUE),
    Acc_Neu_meanRT = mean(c(NeuIrt, NeuSrt), na.rm = TRUE),
    Acc_Pos_meanRT = mean(c(PosIrt, PosSrt), na.rm = TRUE),
    Acc_All_meanRT = mean(c(NegIrt, NegSrt, NeuIrt, NeuSrt, PosIrt, PosSrt), na.rm = TRUE),
    Image_meanRT = mean(c(NegIrt, NeuIrt, PosIrt), na.rm = TRUE),
    NoImage_meanRT = mean(c(NegSrt, NeuSrt, PosSrt), na.rm = TRUE),
    
    
    .groups = 'drop'
  )

# Calculate accuracy percentages
accuracy_data <- EMA %>%
  mutate(across(c(NegIacc, NegSacc, NeuIacc, NeuSacc, PosIacc, PosSacc),
                ~. == 1)) %>%  # Convert to TRUE (correct) FALSE (incorrect)
  group_by(Subject, RewardCond, ControlCond) %>%
  summarize(
    # Accuracy for each condition
    NegI_acc = mean(NegIacc, na.rm = TRUE),
    NegS_acc = mean(NegSacc, na.rm = TRUE),
    NeuI_acc = mean(NeuIacc, na.rm = TRUE),
    NeuS_acc = mean(NeuSacc, na.rm = TRUE),
    PosI_acc = mean(PosIacc, na.rm = TRUE),
    PosS_acc = mean(PosSacc, na.rm = TRUE),
    
    # Combined accuracy measures
    Neg_acc = mean(c(NegIacc, NegSacc), na.rm = TRUE),
    Neu_acc = mean(c(NeuIacc, NeuSacc), na.rm = TRUE),
    Pos_acc = mean(c(PosIacc, PosSacc), na.rm = TRUE),
    Overall_acc = mean(c(NegIacc, NegSacc, NeuIacc, NeuSacc, PosIacc, PosSacc), na.rm = TRUE),
    
    # Image vs No Image accuracy
    Image_acc = mean(c(NegIacc, NeuIacc, PosIacc), na.rm = TRUE),
    NoImage_acc = mean(c(NegSacc, NeuSacc, PosSacc), na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

# Combine all the data into one comprehensive dataframe
comprehensive_data <- subject_data %>%
  left_join(condition_rt_means, by = "Subject") %>%
  left_join(acc_condition_rt_means, by = c("Subject", "RewardCond", "ControlCond")) %>%
  left_join(accuracy_data, by = c("Subject", "RewardCond", "ControlCond"))

#--------------------------------------
#CALCULATE Z SCORES
# Calculate z-scores within each subject and condition
#-------------------------------------------------------------------------------
speed_acc_data <- comprehensive_data %>%
  select(
    Subject, RewardCond, ControlCond,
    # RT measures
    All_meanRT, Acc_All_meanRT,
    Neg_meanRT, Neu_meanRT, Pos_meanRT, NegI_meanRT, NegS_meanRT, PosI_meanRT,
    PosS_meanRT, NeuI_meanRT, NeuS_meanRT, 
    # Accuracy measures
    Overall_acc,
    Neg_acc, Neu_acc, Pos_acc, 
    NegI_acc, NegS_acc, NeuI_acc, NeuS_acc, PosI_acc, PosS_acc
  )
#calculate z-scores within each subject
# Safe z-score function
safe_scale <- function(x) {
  if(all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  x_sd <- sd(x, na.rm = TRUE)
  if(is.na(x_sd) || x_sd == 0) {
    return(rep(0, length(x)))
  }
  if(sum(!is.na(x)) == 1) {
    return(rep(0, length(x)))
  }
  scaled <- as.numeric(scale(x))
  
  # Ensure we return same length as input
  if(length(scaled) != length(x)) {
    return(rep(NA_real_, length(x)))
  }
  
  return(scaled)
}

# Calculate z-scores
speed_acc_z <- speed_acc_data %>%
  mutate(
    z_AllRT = safe_scale(All_meanRT),
    z_OverallAcc = safe_scale(Overall_acc),
    z_NegRT = safe_scale(Neg_meanRT),
    z_NegAcc = safe_scale(Neg_acc),
    z_NeuRT = safe_scale(Neu_meanRT),
    z_NeuAcc = safe_scale(Neu_acc),
    z_PosRT = safe_scale(Pos_meanRT),
    z_PosAcc = safe_scale(Pos_acc),
    z_NegIRT = safe_scale(NegI_meanRT),
    z_NegSRT = safe_scale(NegS_meanRT),
    z_NegIAcc = safe_scale(NegI_acc),
    z_NegSAcc = safe_scale(NegS_acc),
    z_NeuIRT = safe_scale(NeuI_meanRT),
    z_NeuSRT = safe_scale(NeuS_meanRT),
    z_NeuIAcc = safe_scale(NeuI_acc),
    z_NeuSAcc = safe_scale(NeuS_acc),
    z_PosIRT = safe_scale(PosI_meanRT),
    z_PosSRT = safe_scale(PosS_meanRT),
    z_PosSAcc = safe_scale(PosS_acc),
    z_PosIAcc = safe_scale(PosI_acc)
  ) %>%
  ungroup()

# Calculate speed-accuracy tradeoffs
speed_acc_tradeoff <- speed_acc_z %>%
  mutate(
    SAT_Overall = ifelse(is.na(z_AllRT) | is.na(z_OverallAcc), 
                         NA_real_,
                         z_AllRT - z_OverallAcc),
    
    SAT_Neg = ifelse(is.na(z_NegRT) | is.na(z_NegAcc),
                     NA_real_,
                     z_NegRT - z_NegAcc),
    
    SAT_Neu = ifelse(is.na(z_NeuRT) | is.na(z_NeuAcc),
                     NA_real_,
                     z_NeuRT - z_NeuAcc),
    
    SAT_Pos = ifelse(is.na(z_PosRT) | is.na(z_PosAcc),
                     NA_real_,
                     z_PosRT - z_PosAcc),
    SAT_NegImg = ifelse(is.na(z_NegIRT) | is.na(z_NegIAcc),
                        NA_real_,
                        z_NegIRT - z_NegIAcc),
    
    SAT_NeuImg = ifelse(is.na(z_NeuIRT) | is.na(z_NeuIAcc),
                        NA_real_,
                        z_NeuIRT - z_NeuIAcc),
    
    SAT_PosImg = ifelse(is.na(z_PosIRT) | is.na(z_PosIAcc),
                        NA_real_,
                        z_PosRT - z_PosAcc),
    SAT_NegNoImg = ifelse(is.na(z_NegSRT) | is.na(z_NegSAcc),
                          NA_real_,
                          z_NegSRT - z_NegSAcc),
    
    SAT_NeuNoImg = ifelse(is.na(z_NeuSRT) | is.na(z_NeuSAcc),
                          NA_real_,
                          z_NeuSRT - z_NeuSAcc),
    
    SAT_PosNoImg = ifelse(is.na(z_PosSRT) | is.na(z_PosSAcc),
                          NA_real_,
                          z_PosSRT - z_PosSAcc),)


# Interpretation (only add if all SAT scores exist)
SAT_Overall_Interpret = if(exists("SAT_Overall")) {
  case_when(
    is.na(SAT_Overall) ~ "Missing data",
    SAT_Overall > 1 ~ "Strong speed emphasis",
    SAT_Overall > 0 ~ "Speed emphasis",
    SAT_Overall == 0 ~ "Balanced",
    SAT_Overall < -1 ~ "Strong accuracy emphasis",
    SAT_Overall < 0 ~ "Accuracy emphasis"
  )} else NULL

# View results
View(speed_acc_tradeoff)
#-------------------------------------------------------------------------------
#IES
str(comprehensive_data)
IES_scores <- comprehensive_data %>%
  mutate(
    # Overall IES
    IES_Overall = ifelse(is.na(All_meanRT) | is.na(Overall_acc) | Overall_acc == 0, 
                         NA_real_,
                         All_meanRT / Overall_acc),
    IES_Neg = ifelse(is.na(Neg_meanRT) | is.na(Neg_acc) | Neg_acc == 0,
                     NA_real_,
                     Neg_meanRT / Neg_acc),
    IES_Neu = ifelse(is.na(Neu_meanRT) | is.na(Neu_acc) | Neu_acc == 0,
                     NA_real_,
                     Neu_meanRT / Neu_acc),
    IES_Pos = ifelse(is.na(Pos_meanRT) | is.na(Pos_acc) | Pos_acc == 0,
                     NA_real_,
                     Pos_meanRT / Pos_acc),
    IES_NegImg = ifelse(is.na(NegI_meanRT) | is.na(NegI_acc) | NegI_acc == 0,
                        NA_real_,
                        NegI_meanRT / NegI_acc),
    IES_NeuImg = ifelse(is.na(NeuI_meanRT) | is.na(NeuI_acc) | NeuI_acc == 0,
                        NA_real_,
                        NeuI_meanRT / NeuI_acc),
    
    IES_PosImg = ifelse(is.na(PosI_meanRT) | is.na(PosI_acc) | PosI_acc == 0,
                        NA_real_,
                        PosI_meanRT / PosI_acc),
    IES_NegNoImg = ifelse(is.na(NegS_meanRT) | is.na(NegS_acc) | NegS_acc == 0,
                          NA_real_,
                          NegS_meanRT / NegS_acc),
    IES_NeuNoImg = ifelse(is.na(NeuS_meanRT) | is.na(NeuS_acc) | NeuS_acc == 0,
                          NA_real_,
                          NeuS_meanRT / NeuS_acc),
    IES_PosNoImg = ifelse(is.na(PosS_meanRT) | is.na(PosS_acc) | PosS_acc == 0,
                          NA_real_,
                          PosS_meanRT / PosS_acc)
  )
str(IES_scores)

