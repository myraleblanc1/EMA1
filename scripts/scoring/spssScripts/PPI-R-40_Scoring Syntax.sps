* Encoding: UTF-8.
*recode reverse scored items (R=recoded variable).
RECODE PPI02 (1=4) (2=3) (3=2) (4=1) INTO PPI02R.
RECODE PPI06 (1=4) (2=3) (3=2) (4=1) INTO PPI06R.
RECODE PPI07 (1=4) (2=3) (3=2) (4=1) INTO PPI07R.
RECODE PPI14 (1=4) (2=3) (3=2) (4=1) INTO PPI14R.
RECODE PPI17 (1=4) (2=3) (3=2) (4=1) INTO PPI17R.
RECODE PPI18 (1=4) (2=3) (3=2) (4=1) INTO PPI18R.
RECODE PPI22 (1=4) (2=3) (3=2) (4=1) INTO PPI22R.
RECODE PPI23 (1=4) (2=3) (3=2) (4=1) INTO PPI23R.
RECODE PPI24 (1=4) (2=3) (3=2) (4=1) INTO PPI24R.
RECODE PPI25 (1=4) (2=3) (3=2) (4=1) INTO PPI25R.
RECODE PPI26 (1=4) (2=3) (3=2) (4=1) INTO PPI26R.
RECODE PPI27 (1=4) (2=3) (3=2) (4=1) INTO PPI27R.
RECODE PPI29 (1=4) (2=3) (3=2) (4=1) INTO PPI29R.
RECODE PPI30 (1=4) (2=3) (3=2) (4=1) INTO PPI30R.
RECODE PPI32 (1=4) (2=3) (3=2) (4=1) INTO PPI32R.
RECODE PPI36 (1=4) (2=3) (3=2) (4=1) INTO PPI36R.
RECODE PPI39 (1=4) (2=3) (3=2) (4=1) INTO PPI39R.
EXECUTE.

*calculate subscale totals.
COMPUTE Blame_externalization = SUM(PPI04, PPI05, PPI12, PPI21, PPI31).
EXECUTE.

COMPUTE Carefree_nonplanfulness = SUM(PPI23R, PPI25R, PPI30R, PPI32R, PPI36R).
EXECUTE.

COMPUTE Coldheartedness = SUM(PPI07R, PPI17R, PPI24R, PPI26R, PPI39R).
EXECUTE.

COMPUTE Fearlessness = SUM(PPI03, PPI14R, PPI28, PPI34, PPI37).
EXECUTE.

COMPUTE Machiavellian_egocentricity = SUM(PPI09, PPI16, PPI19, PPI33, PPI40).
EXECUTE.

COMPUTE Rebellious_nonconformity = SUM(PPI01, PPI11, PPI15, PPI20, PPI38).
EXECUTE.

COMPUTE Social_influence = SUM(PPI06R, PPI10, PPI13, PPI22R, PPI27R).
EXECUTE.

COMPUTE Stress_immunity = SUM(PPI02R, PPI08, PPI18R, PPI29R, PPI35).
EXECUTE.

*calculate factors (SCI = self-centered impulsivity; FD = fearless dominance; coldheartedness factor = coldheartedness subscale).  
    
COMPUTE SCI = SUM(Machiavellian_egocentricity, Rebellious_nonconformity, Blame_externalization, Carefree_nonplanfulness).
EXECUTE. 

COMPUTE FD = SUM(Social_influence, Fearlessness, Stress_immunity). 
EXECUTE. 

*calculate total score.
COMPUTE PPIR40_Total = SUM(Blame_externalization, Carefree_nonplanfulness, Coldheartedness, Fearlessness, Machiavellian_egocentricity, Rebellious_nonconformity, Social_influence, Stress_immunity).
EXECUTE.

