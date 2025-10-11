* Encoding: UTF-8.
*calculate total score. (command if all items coded 0-3 originally, including items 16 and 18).
COMPUTE BDI_Total = SUM(BDI01, BDI02, BDI03, BDI04, BDI05, BDI06, BDI07, BDI08, BDI09, BDI10, BDI11, BDI12, BDI13, BDI14, BDI15, BDI16, BDI17, BDI18, BDI19, BDI20, BDI21).
EXECUTE.



*recode if items 16 and 18 coded 0-6 originally. if so, ignore command above. (R = recoded variable). 
RECODE BDI16 (0=0) (1=1) (2=1) (3=2) (4=2) (5=3) (6=3) INTO BDI16R.
RECODE BDI18 (0=0) (1=1) (2=1) (3=2) (4=2) (5=3) (6=3) INTO BDI18R. 
EXECUTE.

*calculate total score. (command, after recoding, if items 16 and 18 coded 0-6 originally). 
COMPUTE BDI_Total = SUM(BDI01, BDI02, BDI03, BDI04, BDI05, BDI06, BDI07, BDI08, BDI09, BDI10, BDI11, BDI12, BDI13, BDI14, BDI15, BDI16R, BDI17, BDI18R, BDI19, BDI20, BDI21).
EXECUTE.



*recode if ALL items coded starting at 1, including items 16 and 18. if so, ignore all commands above. (R = recoded variable). 
RECODE BDI01 (1=0) (2=1) (3=2) (4=3) INTO BDI01R.
RECODE BDI02 (1=0) (2=1) (3=2) (4=3) INTO BDI02R.
RECODE BDI03 (1=0) (2=1) (3=2) (4=3) INTO BDI03R.
RECODE BDI04 (1=0) (2=1) (3=2) (4=3) INTO BDI04R.
RECODE BDI05 (1=0) (2=1) (3=2) (4=3) INTO BDI05R.
RECODE BDI06 (1=0) (2=1) (3=2) (4=3) INTO BDI06R. 
RECODE BDI07 (1=0) (2=1) (3=2) (4=3) INTO BDI07R.
RECODE BDI08 (1=0) (2=1) (3=2) (4=3) INTO BDI08R.
RECODE BDI09 (1=0) (2=1) (3=2) (4=3) INTO BDI09R.
RECODE BDI10 (1=0) (2=1) (3=2) (4=3) INTO BDI10R.
RECODE BDI11 (1=0) (2=1) (3=2) (4=3) INTO BDI11R.
RECODE BDI12 (1=0) (2=1) (3=2) (4=3) INTO BDI12R.
RECODE BDI13 (1=0) (2=1) (3=2) (4=3) INTO BDI13R.
RECODE BDI14 (1=0) (2=1) (3=2) (4=3) INTO BDI14R.
RECODE BDI15 (1=0) (2=1) (3=2) (4=3) INTO BDI15R. 
RECODE BDI16 (1=0) (2=1) (3=1) (4=2) (5=2) (6=3) (7=3) INTO BDI16R. 
RECODE BDI17 (1=0) (2=1) (3=2) (4=3) INTO BDI17R.
RECODE BDI18 (1=0) (2=1) (3=1) (4=2) (5=2) (6=3) (7=3) INTO BDI18R.
RECODE BDI19 (1=0) (2=1) (3=2) (4=3) INTO BDI19R.
RECODE BDI20 (1=0) (2=1) (3=2) (4=3) INTO BDI20R.
RECODE BDI21 (1=0) (2=1) (3=2) (4=3) INTO BDI21R. 
EXECUTE.    

*calculate total score. (command, after recoding, if ALL items coded starting at 1 originally).
COMPUTE BDI_Total = SUM(BDI01R, BDI02R, BDI03R, BDI04R, BDI05R, BDI06R, BDI07R, BDI08R, BDI09R, BDI10R, BDI11R, BDI12R, BDI13R, BDI14R, BDI15R, BDI16R, BDI17R, BDI18R, BDI19R, BDI20R, BDI21R).
EXECUTE.


  

