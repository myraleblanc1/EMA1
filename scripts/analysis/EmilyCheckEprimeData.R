#Shape Emily's Inspection Data

library(tidyverse)
#IMPORTING ALL DATA

#import eprime
EmilyData <- read_delim(
  'data/raw/data_0929.txt',   # The name of your *.txt file output from Emerge
  delim = '\t', # These data are tab separated
) 

#Clean E-prime Data
EmilyData_clean <- 
  EmilyData %>%                  # original data
  select(
    Subject = Subject,            # subject
    KResp = KResp,              # counterbalanced keys, K,N
    NResp = NResp,              # 
    NegImgacc = PracResp1.ACC,    # Neg Image present accuracy
    NegImgrt = PracResp1.RT,      # Neg Image present rt
    NegImgKey = PracResp1.RESP,   # Neg Image present key
    NegNoImgacc = PracResp2.ACC,    # Neg noImage present accuracy
    NegNoImgrt = PracResp2.RT,      # Neg noImage present rt
    NegNoImgKey = PracResp2.RESP,   # Neg noImage present key
    NeuImgacc = PracResp11.ACC,   # Neu Image present accuracy
    NeuImgrt = PracResp11.RT,     # Neu Image present rt
    NeuImgKey = PracResp11.RESP,   # Neu Image present key
    NeuNoImgacc = PracResp12.ACC,   # Neu noImage present accuracy
    NeuNoImgrt = PracResp12.RT,     # Neu noImage present rt
    NeuNoImgKey = PracResp12.RESP,   # Neu noImage present key
    PosImgacc = PracResp9.ACC,    # Pos Image present accuracy
    PosImgrt = PracResp9.RT,      # Pos Image present rt
    PosImgKey = PracResp9.RESP,   # Pos Image present key
    PosNoImgacc = PracResp10.ACC,   # Pos noImage present accuracy
    PosNoImgrt = PracResp10.RT,     # Pos noImage present rt
    PosNoImgKey = PracResp10.RESP,   # Pos noImage present key
  )

str(EmilyData_clean)
View(EmilyData_clean)

write.csv(EmilyData_clean, "data/processed/Emily_Eprime_inspection.csv", row.names = FALSE)
