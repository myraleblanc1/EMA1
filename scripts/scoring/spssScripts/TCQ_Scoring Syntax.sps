* Encoding: UTF-8.
#recode reverse scored items

DATASET ACTIVATE DataSet1.
RECODE TCQ_5 (1=4) (2=3) (3=2) (4=1) INTO TCQ_5.
RECODE TCQ_8 (1=4) (2=3) (3=2) (4=1) INTO TCQ_8.
RECODE TCQ_12 (1=4) (2=3) (3=2) (4=1) INTO TCQ_12.
EXECUTE.

#Thought control total

DATASET ACTIVATE DataSet1.
compute TCQ_Total = sum(TCQ_1,TCQ_2,TCQ_3,TCQ_4,TCQ_5,TCQ_6,TCQ_7,TCQ_8,TCQ_9,TCQ_10,TCQ_11,TCQ_12,TCQ_13,TCQ_14,TCQ_15,TCQ_16,TCQ_17,TCQ_18,TCQ_19,TCQ_20,TCQ_21,TCQ_22,TCQ_23,TCQ_24,TCQ_25,TCQ_26,TCQ_27,TCQ_28,TCQ_29,TCQ_30).
EXECUTE.

#Distraction subscale

DATASET ACTIVATE DataSet1.
compute Distraction = sum(TCQ_1,TCQ_9,TCQ_16,TCQ_19,TCQ_21,TCQ_30).
EXECUTE.

#Punishment subscale

DATASET ACTIVATE DataSet1.
compute Punishment = sum(TCQ_2,TCQ_6,TCQ_11,TCQ_13,TCQ_15,TCQ_28).
EXECUTE.
 
#Reappraisal subscale

DATASET ACTIVATE DataSet1.
compute Reappraisal = sum(TCQ_3,TCQ_10,TCQ_14,TCQ_20,TCQ_23,TCQ_27).
EXECUTE.

#Worry subscale
 
DATASET ACTIVATE DataSet1.
compute Worry = sum(TCQ_4,TCQ_7,TCQ_18,TCQ_22,TCQ_24,TCQ_26).
EXECUTE.

#Social control subscale

DATASET ACTIVATE DataSet1.
compute Social_Control = sum(TCQ_5,TCQ_8,TCQ_12,TCQ_17,TCQ_25,TCQ_29).
EXECUTE.
