#IMPORT THE DATA AND CLEAN IT FOR ANALYSIS
library(tidyverse)
library(dplyr)
library(readr)

#-------------------------------------------------------------------------------
#IMPORTING ALL DATA

#import eprime 09 29 (roughly 100 participants)
posterData <- read_delim(
  'data/raw/FINAL_0929.txt',   # The name of your *.txt file output from Emerge
  delim = '\t', # These data are tab separated
) 



#Clean E-prime Data
posterData_clean <- 
  posterData |>                  
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
Qualtrics <- read.csv("data/raw/Qualtrics0929.csv")

#Clean Qualtrics
Qualtrics_clean <- 
  Qualtrics |>                 
  select(
    Subject = Q247,
    Age = Q2,
    Ethinicity = Q3,
    Gender = Q1,
    PPIR40 = SC1,
  )

#import eprime excel w/ conds
EprimeFiles <- read.csv("data/raw/EMA1_eprime_files_0929.csv")

#Clean EprimeFiles
EprimeFiles_clean <- 
  EprimeFiles |>                  
  select(
    Subject = Subject..,
    ControlCond = Control.Condition,
    RewardCond = Reward.Condition,
  )

#-------------------------------------------------------------------------------

#make all Subjects the same data type 
posterData_clean$Subject <- as.character(posterData_clean$Subject)
Qualtrics_clean$Subject <- as.character(Qualtrics_clean$Subject)
EprimeFiles_clean$Subject <- as.character(EprimeFiles$Subject)


Qualtrics_clean <- Qualtrics_clean |>
  distinct(Subject, .keep_all = TRUE)

EprimeFiles_clean <- EprimeFiles_clean |>
  distinct(Subject, .keep_all = TRUE)

#merge datasets by subject
joinedData <- posterData_clean |>
  inner_join(Qualtrics_clean, by = "Subject") |>
  inner_join(EprimeFiles_clean, by = "Subject")

#-------------------------------------------------------------------------------
#Fix all datatypes for final dataset
#Factor
EMA_factors <- joinedData |>
  mutate(across(
    .cols = c(A1Block, A2Block, B1Block, B2Block, 
               Weight1, Weight2, 
              Subject, Ethinicity, Gender, ControlCond,
              RewardCond, KResp, NResp),
    .fns = factor
  ))

#number
#prints out any non-numerical values entered in age on Qualtrics
EMA_factors |>
  +     select(Age, PPIR40, NegIacc, NegSacc) |>
  +     filter(!grepl("^[0-9.]+$", Age)) |>   # shows rows where Age isn't purely numeric
  +     distinct(Age)
#remove the strings
EMA_factors <- EMA_factors |>
  mutate(Age = parse_number(Age))

EMA <- EMA_factors |>
  mutate(across(
    .cols = c(Age, PPIR40, NegIacc, NegSacc, NeuIacc, NeuSacc, 
              PosIacc, PosSacc,),
    .fns = as.numeric
  ))

#-------------------------------------------------------------------------------
# Create summary datasets and save them

# Create subject demographic data
subject_data <- EMA |>
  select(Subject, Age, Gender, Ethinicity, PPIR40, ControlCond, RewardCond) |>
  distinct(Subject, .keep_all = TRUE)

# Create dataset with only accurate trials
accEMA <- EMA |>
  filter(
    (NegIacc == 1 | is.na(NegIacc)),
    (NegSacc == 1 | is.na(NegSacc)),
    (NeuIacc == 1 | is.na(NeuIacc)),
    (NeuSacc == 1 | is.na(NeuSacc)),
    (PosIacc == 1 | is.na(PosIacc)),
    (PosSacc == 1 | is.na(PosSacc))
  )


# Calculate accuracy percentages
accuracy_data <- EMA |>
  mutate(across(c(NegIacc, NegSacc, NeuIacc, NeuSacc, PosIacc, PosSacc),
                ~. == 1)) |>  # Convert to TRUE (correct) FALSE (incorrect)
  group_by(Subject, RewardCond, ControlCond) |>
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
  ) |>
  mutate(across(where(is.numeric), ~round(., 3)))

