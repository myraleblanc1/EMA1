#Power Analysis
library(dplyr)
library(tidyr)
between_clean_final <- read.csv("data/processed/between_clean_final.csv")

between_clean_final$Subject <- factor(between_clean_final$Subject)

between_clean_final$Subject     <- factor(between_clean_final$Subject)
between_clean_final$valence     <- factor(between_clean_final$valence)
between_clean_final$ControlCond <- factor(between_clean_final$ControlCond)
between_clean_final$RewardCond  <- factor(between_clean_final$RewardCond)

pilot_clean <- subset(
  between_clean_final,
  !is.na(RT) & !is.na(valence) & 
    !is.na(ControlCond) & !is.na(RewardCond)
)

library(afex)

aov_pilot <- aov_ez(
  id = "Subject",
  dv = "RT",
  data = pilot_clean,
  within = "valence",
  between = c("ControlCond", "RewardCond")
)

summary(aov_pilot)

library(effectsize)

eta_out <- eta_squared(aov_pilot, partial = TRUE)
eta_out

etas_f <- effectsize_conversion(
  eta_out$Eta2_partial,
  from = "eta",
  to = "f"
)
etas_f

eta_out

########################################
### POWER FOR 3×2×2 ANOVA INTERACTION
### (using pilot eta² = 0.03)
########################################

# Effect size conversion
eta <- 0.03
f <- sqrt(eta / (1 - eta))
f   # print effect size

# Run power analysis for 80% power
pwr::pwr.anova.test(
  k = 12,            # 3×2×2 = 12 cells
  f = f,             # effect size from pilot
  sig.level = 0.05,
  power = 0.80
)
