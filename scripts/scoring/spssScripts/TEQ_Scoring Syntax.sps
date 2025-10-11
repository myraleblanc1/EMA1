* Encoding: UTF-8.
*recode reverse scored items (R=recoded variable).
RECODE TEQ02 (0=4) (1=3) (2=2) (3=1) (4=0) INTO TEQ02R.
RECODE TEQ04 (0=4) (1=3) (2=2) (3=1) (4=0) INTO TEQ04R.
RECODE TEQ07 (0=4) (1=3) (2=2) (3=1) (4=0) INTO TEQ07R.
RECODE TEQ10 (0=4) (1=3) (2=2) (3=1) (4=0) INTO TEQ10R.
RECODE TEQ11 (0=4) (1=3) (2=2) (3=1) (4=0) INTO TEQ11R.
RECODE TEQ12 (0=4) (1=3) (2=2) (3=1) (4=0) INTO TEQ12R.
RECODE TEQ14 (0=4) (1=3) (2=2) (3=1) (4=0) INTO TEQ14R.
RECODE TEQ15 (0=4) (1=3) (2=2) (3=1) (4=0) INTO TEQ15R.
EXECUTE.

*calculate total score.
COMPUTE TEQ_Total = SUM(TEQ01, TEQ02R, TEQ03, TEQ04R, TEQ05, TEQ06, TEQ07R, TEQ08, TEQ09, TEQ10R, TEQ11R, TEQ12R, TEQ13, TEQ14R, TEQ15R, TEQ16).
EXECUTE. 



*recode ALL items if ALL items coded starting at 1 (this includes reverse scoring). if so, ignore all commands above. (R = recoded variable).
RECODE TEQ01 (1=0) (2=1) (3=2) (4=3) (5=4) INTO TEQ01R.
RECODE TEQ02 (1=4) (2=3) (3=2) (4=1) (5=0) INTO TEQ02R.
RECODE TEQ03 (1=0) (2=1) (3=2) (4=3) (5=4) INTO TEQ03R.
RECODE TEQ04 (1=4) (2=3) (3=2) (4=1) (5=0) INTO TEQ04R.
RECODE TEQ05 (1=0) (2=1) (3=2) (4=3) (5=4) INTO TEQ05R.
RECODE TEQ06 (1=0) (2=1) (3=2) (4=3) (5=4) INTO TEQ06R.
RECODE TEQ07 (1=4) (2=3) (3=2) (4=1) (5=0) INTO TEQ07R.
RECODE TEQ08 (1=0) (2=1) (3=2) (4=3) (5=4) INTO TEQ08R.
RECODE TEQ09 (1=0) (2=1) (3=2) (4=3) (5=4) INTO TEQ09R.
RECODE TEQ10 (1=4) (2=3) (3=2) (4=1) (5=0) INTO TEQ10R.
RECODE TEQ11 (1=4) (2=3) (3=2) (4=1) (5=0) INTO TEQ11R.
RECODE TEQ12 (1=4) (2=3) (3=2) (4=1) (5=0) INTO TEQ12R.
RECODE TEQ13 (1=0) (2=1) (3=2) (4=3) (5=4) INTO TEQ13R.
RECODE TEQ14 (1=4) (2=3) (3=2) (4=1) (5=0) INTO TEQ14R.
RECODE TEQ15 (1=4) (2=3) (3=2) (4=1) (5=0) INTO TEQ15R.
RECODE TEQ16 (1=0) (2=1) (3=2) (4=3) (5=4) INTO TEQ16R.
EXECUTE. 

*calculate total score. (command, after recoding/reverse scoring, if ALL items coding starting at 1 originally). 
COMPUTE TEQ_Total = SUM(TEQ01R, TEQ02R, TEQ03R, TEQ04R, TEQ05R, TEQ06R, TEQ07R, TEQ08R, TEQ09R, TEQ10R, TEQ11R, TEQ12R, TEQ13R, TEQ14R, TEQ15R, TEQ16R).
EXECUTE. 
