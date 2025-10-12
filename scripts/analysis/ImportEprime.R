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

#Clean E-prime Data
posterData_clean <- 
  posterData %>%                  # original data
  select(
    Counterbalance = Subject,
    Subject = Session,            # subject
    A1Block = A1Block,          # Block order: A1B1B1A1 A2B2B2A2
    A2Block = A2Block,          # 
    B1Block = B1Block,          # 
    B2Block = B2Block,          # 
    KResp = KResp,              # counterbalanced keys, K,N
    NResp = NResp,              # 
    Weight1 = Weight1,          # Proactive vs Reactive cond
    Weight2 = Weight2,          # 
    NegIacc = PracResp1.ACC,    # Neg Image present accuracy
    NegIrt = PracResp1.RT,      # Neg Image present rt
    NegSacc = PracResp2.ACC,    # Neg noImage present accuracy
    NegSrt = PracResp2.RT,      # Neg noImage present rt
    NeuIacc = PracResp11.ACC,   # Neu Image present accuracy
    NeuIrt = PracResp11.RT,     # Neu Image present rt
    NeuSacc = PracResp12.ACC,   # Neu noImage present accuracy
    NeuSrt = PracResp12.RT,     # Neu noImage present rt
    PosIacc = PracResp9.ACC,    # Pos Image present accuracy
    PosIrt = PracResp9.RT,      # Pos Image present rt
    PosSacc = PracResp10.ACC,   # Pos noImage present accuracy
    PosSrt = PracResp10.RT,     # Pos noImage present rt
    FixDur = FixDuration,
    ITIDur = ITIDuration,
  )

str(posterData_clean)

#import Qualtrics
Qualtrics <- read.csv("data/Qualtrics.csv")

#Clean Qualtrics
Qualtrics_clean <- 
  Qualtrics %>%                  # original data
  select(
         Subject = Subject,
         Age = Q2,
         Ethinicity = Q3,
         Gender = Q1,
         PPIR40 = SC1,
  )

str(Qualtrics)

#import eprime excel w/ conds
EprimeFiles <- read.csv("data/EMA1_eprime_files.csv")

#Clean EprimeFiles
EprimeFiles_clean <- 
  EprimeFiles %>%                  # original data
  select(
    Subject = Subject,
    ControlCond = Control.Condition,
    RewardCond = Reward.Condition,
  )
str(EprimeFiles_clean)
#-------------------------------------------------------------------------------

#make all Subjects the same data type 
#!!!!character makes subsequent inner_join work, changed to factor AFTER!!!!
posterData_clean$Subject <- as.character(posterData_clean$Subject)
Qualtrics_clean$Subject <- as.character(Qualtrics$Subject)
EprimeFiles_clean$Subject <- as.character(EprimeFiles$Subject)


#merge datasets by subject
joinedData <- Qualtrics_clean %>%
  inner_join(posterData_clean, by = "Subject") %>%
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
    .cols = c(Age, PPIR40
              ),
    .fns = as.numeric
  ))


# Create final dataset with only 1 row / participant 
subject_data <- EMA %>%
  # Select only the columns that are consistent across trials
  select(Subject, Age, Gender, Ethinicity, PPIR40) %>%  # include other needed demographics
  distinct(Subject, .keep_all = TRUE)  # Keep just one row per participant

#Create dataset with only accurate trials
accEMA <- EMA %>%
  filter(
    (NegIacc == 1 | is.na(NegIacc)),
    (NegSacc == 1 | is.na(NegSacc)),
    (NeuIacc == 1 | is.na(NeuIacc)),
    (NeuSacc == 1 | is.na(NeuSacc)),
    (PosIacc == 1 | is.na(PosIacc)),
    (PosSacc == 1 | is.na(PosSacc))
  )
#final dataset with means per rt oif all trials
subject_means <- EMA %>%
  group_by(RewardCond, Subject) %>%
  summarise(
    mean_NegIrt = mean(NegIrt, na.rm = TRUE),
    mean_NegSrt = mean(NegSrt, na.rm = TRUE),
    mean_NeuIrt = mean(NeuIrt, na.rm = TRUE),
    mean_NeuSrt = mean(NeuSrt, na.rm = TRUE),
    mean_PosIrt = mean(PosIrt, na.rm = TRUE),
    mean_PosSrt = mean(PosSrt, na.rm = TRUE),
    mean_PPIR40 = mean(PPIR40, na.rm = TRUE),
    mean_Negrt = mean(c(NegIrt, NegSrt), na.rm = TRUE),  
    mean_Neurt = mean(c(NeuIrt, NeuSrt), na.rm = TRUE), 
    mean_Posrt = mean(c(PosIrt, PosSrt), na.rm = TRUE),
    .groups = 'drop'
  ) 
#---------------control
consubject_means <- EMA %>%
  group_by(ControlCond, Subject) %>%
  summarise(
    mean_NegIrt = mean(NegIrt, na.rm = TRUE),
    mean_NegSrt = mean(NegSrt, na.rm = TRUE),
    mean_NeuIrt = mean(NeuIrt, na.rm = TRUE),
    mean_NeuSrt = mean(NeuSrt, na.rm = TRUE),
    mean_PosIrt = mean(PosIrt, na.rm = TRUE),
    mean_PosSrt = mean(PosSrt, na.rm = TRUE),
    mean_PPIR40 = mean(PPIR40, na.rm = TRUE),
    mean_Negrt = mean(c(NegIrt, NegSrt), na.rm = TRUE),  
    mean_Neurt = mean(c(NeuIrt, NeuSrt), na.rm = TRUE), 
    mean_Posrt = mean(c(PosIrt, PosSrt), na.rm = TRUE),
    .groups = 'drop'
  ) 

str(subject_means)

#final dataset with means per rt of just accurate trials
accsubject_means <- accEMA %>%
  group_by(RewardCond, Subject) %>%
  summarise(
    mean_NegIrt = mean(NegIrt, na.rm = TRUE),
    mean_NegSrt = mean(NegSrt, na.rm = TRUE),
    mean_NeuIrt = mean(NeuIrt, na.rm = TRUE),
    mean_NeuSrt = mean(NeuSrt, na.rm = TRUE),
    mean_PosIrt = mean(PosIrt, na.rm = TRUE),
    mean_PosSrt = mean(PosSrt, na.rm = TRUE),
    mean_PPIR40 = mean(PPIR40, na.rm = TRUE),
    mean_Negrt = mean(c(NegIrt, NegSrt), na.rm = TRUE),  
    mean_Neurt = mean(c(NeuIrt, NeuSrt), na.rm = TRUE), 
    mean_Posrt = mean(c(PosIrt, PosSrt), na.rm = TRUE),
    .groups = 'drop'
  ) 
#calculate percentages

accPercEMA <- EMA %>%
  mutate(across(c(NegIacc, NegSacc, NeuIacc, NeuSacc, PosIacc, PosSacc),
                ~. == 1)) %>%  # Convert to TRUE (correct) FALSE (incorrect)
  group_by(Subject) %>%
  summarize(
    across(c(NegIacc, NegSacc, NeuIacc, NeuSacc, PosIacc, PosSacc),
           ~mean(., na.rm = TRUE),
           .names = "{.col}_acc"),
    PPIR40 = first(na.omit(PPIR40)),
    Overall_acc = mean(c(NegIacc, NegSacc, NeuIacc, NeuSacc, PosIacc, PosSacc), na.rm = TRUE),
    # Combined accuracy for image conditions (NegIacc, PosIacc, NeuIacc)
    imgacc = mean(c(NegIacc, PosIacc, NeuIacc), na.rm = TRUE)
  ) %>%
  mutate(across(ends_with("_acc"), ~round(., 3)))

str(accPercEMA)

str(accsubject_means)
subject_means <- EMA %>%
  group_by(RewardCond, Subject) %>%
  summarise(
    mean_NegIrt = mean(NegIrt, na.rm = TRUE),
    mean_NegSrt = mean(NegSrt, na.rm = TRUE),
    mean_NeuIrt = mean(NeuIrt, na.rm = TRUE),
    mean_NeuSrt = mean(NeuSrt, na.rm = TRUE),
    mean_PosIrt = mean(PosIrt, na.rm = TRUE),
    mean_PosSrt = mean(PosSrt, na.rm = TRUE),
    mean_PPIR40 = mean(PPIR40, na.rm = TRUE),
    mean_Negrt = mean(c(NegIrt, NegSrt), na.rm = TRUE),  
    mean_Neurt = mean(c(NeuIrt, NeuSrt), na.rm = TRUE), 
    mean_Posrt = mean(c(PosIrt, PosSrt), na.rm = TRUE),
    .groups = 'drop'
  ) 
#---------------------
#control COnd

Conaccsubject_means <- accEMA %>%
  group_by(ControlCond, Subject) %>%
  summarise(
    mean_NegIrt = mean(NegIrt, na.rm = TRUE),
    mean_NegSrt = mean(NegSrt, na.rm = TRUE),
    mean_NeuIrt = mean(NeuIrt, na.rm = TRUE),
    mean_NeuSrt = mean(NeuSrt, na.rm = TRUE),
    mean_PosIrt = mean(PosIrt, na.rm = TRUE),
    mean_PosSrt = mean(PosSrt, na.rm = TRUE),
    mean_PPIR40 = mean(PPIR40, na.rm = TRUE),
    mean_Negrt = mean(c(NegIrt, NegSrt), na.rm = TRUE),  
    mean_Neurt = mean(c(NeuIrt, NeuSrt), na.rm = TRUE), 
    mean_Posrt = mean(c(PosIrt, PosSrt), na.rm = TRUE),
    .groups = 'drop'
  ) 
#calculate percentages

ConaccPercEMA <- EMA %>%
  mutate(across(c(NegIacc, NegSacc, NeuIacc, NeuSacc, PosIacc, PosSacc),
                ~. == 1)) %>%  # Convert to TRUE (correct) FALSE (incorrect)
  group_by(Subject, ControlCond) %>%
  summarize(
    across(c(NegIacc, NegSacc, NeuIacc, NeuSacc, PosIacc, PosSacc),
           ~mean(., na.rm = TRUE),
           .names = "{.col}_acc"),
    PPIR40 = first(na.omit(PPIR40)),
    Overall_acc = mean(c(NegIacc, NegSacc, NeuIacc, NeuSacc, PosIacc, PosSacc), na.rm = TRUE),
    # Combined accuracy for image conditions (NegIacc, PosIacc, NeuIacc)
    imgacc = mean(c(NegIacc, PosIacc, NeuIacc), na.rm = TRUE)
  ) %>%
  mutate(across(ends_with("_acc"), ~round(., 3)))

str(ConaccPercEMA)

str(Conaccsubject_means)
Consubject_means <- EMA %>%
  group_by(ControlCond, Subject) %>%
  summarise(
    mean_NegIrt = mean(NegIrt, na.rm = TRUE),
    mean_NegSrt = mean(NegSrt, na.rm = TRUE),
    mean_NeuIrt = mean(NeuIrt, na.rm = TRUE),
    mean_NeuSrt = mean(NeuSrt, na.rm = TRUE),
    mean_PosIrt = mean(PosIrt, na.rm = TRUE),
    mean_PosSrt = mean(PosSrt, na.rm = TRUE),
    mean_PPIR40 = mean(PPIR40, na.rm = TRUE),
    mean_Negrt = mean(c(NegIrt, NegSrt), na.rm = TRUE),  
    mean_Neurt = mean(c(NeuIrt, NeuSrt), na.rm = TRUE), 
    mean_Posrt = mean(c(PosIrt, PosSrt), na.rm = TRUE),
    .groups = 'drop'
  ) 



