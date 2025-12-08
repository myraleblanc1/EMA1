library(dplyr)
library(afex)
library(emmeans)

#-----------------------------------------
# 1) Prepare data for image trials only
#-----------------------------------------
dat_img <- between_clean_final %>%
  filter(presence == "img", !is.na(RT), !is.na(PPIR40)) %>%
  mutate(
    Subject     = factor(Subject),
    RewardCond  = factor(RewardCond),
    ControlCond = factor(ControlCond),
    valence     = factor(valence, levels = c("neg","neu","pos")),
    # force numeric type (no attributes, no factor!)
    PPIR40_c    = as.numeric(PPIR40) - mean(as.numeric(PPIR40), na.rm = TRUE)
  )

# Verify it's numeric
str(dat_img$PPIR40_c)
