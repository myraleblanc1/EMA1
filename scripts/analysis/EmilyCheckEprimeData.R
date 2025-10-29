#Shape Emily's Inspection Data
#IMPORTING ALL DATA

#import eprime
EmilyData <- read_delim(
  'data/data_0929.txt',   # The name of your *.txt file output from Emerge
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