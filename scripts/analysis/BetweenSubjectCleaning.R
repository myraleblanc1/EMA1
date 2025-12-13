#between subject cleaning
within_subj_data <- read.csv("data/processed/clean_accEMA_3SD.csv")
str(within_subj_data)
#-------------------------------------------------------------------------------
library(dplyr)

within_subj_means <- within_subj_data %>%
  group_by(Subject, RewardCond, ControlCond) %>%
  summarize(mean_RT = mean(RT, na.rm = TRUE), .groups = "drop", .keep = "all")

between_clean <- within_subj_means %>%
  group_by(RewardCond, ControlCond) %>%
  mutate(
    grp_mu = mean(mean_RT, na.rm = TRUE),
    grp_sd = sd(mean_RT, na.rm = TRUE),
    keep = mean_RT >= (grp_mu - 3 * grp_sd) & mean_RT <= (grp_mu + 3 * grp_sd)
  ) %>%
  ungroup()
#-------------------------------------------------------
outliers_between <- between_clean %>% 
  group_by(Subject) |>
  filter(!keep)
View(outliers_between)

between_clean_final <- within_subj_data %>%
  filter(!Subject %in% c(132))
str(between_clean_final)
View(between_clean_final)

write.csv(between_clean_final, "data/processed/between_clean_final.csv", row.names = FALSE)
