#TEST PIPLINE LOAD IN DATA SCRIPT 
#-------------------------------------------------------------------------------
library(tidyverse)

#LOAD IN THE DATA 
################################################################################
#MOVE ALL RAW DATA FILES INTO data/raw folder (you can drag and drop)
#REPLACE FILENAME WITH THE ACTUAL FILENAME OF YOUR DATA

raw_Qualtricsdata <- read_csv("data/raw/mastertestdata.csv")

#Remove unecessary Qualtrics output
raw_Qualtricsdata <- raw_Qualtricsdata[-c(2), ]
colnames(raw_Qualtricsdata) <- make.names(as.character(raw_Qualtricsdata[1, ]), unique = TRUE)
raw_Qualtricsdata <- raw_Qualtricsdata[-1, ]
raw_Qualtricsdata <- raw_Qualtricsdata <- raw_Qualtricsdata[, -(1:17)]

#the dataframe names must stay the same for subsequent code to work properly 
#(e.g. raw_behaviouraldata)
################################################################################

#VIEW THE IMPORTED DATA
View(raw_Qualtricsdata)


head(raw_Qualtricsdata)

#-------------------------------------------------------------------------------
#MERGE DATASETS TOGETHER

#To merge all the datasets together they have to have the same column name for the subject ID.
#Here we rename the subject ID column in each dataset to "subject_id" for consistency.
#Change "COLUMN_NAME" to the actual column name in your datasets if it's different 
#Leave this as a string (keep the "")
#In QUALTRICS  this will likely output as Q(##) 
raw_Qualtricsdata <- raw_Qualtricsdata |> rename(subject_id = "Subject.Number.entry")

#Ensure this is changed

View(raw_Qualtricsdata)

rawdata <- raw_Qualtricsdata


#SAVE THE MERGED DATA (as a .csv file)
write_csv(rawdata, paste0("data/raw/rawdata.csv"))

