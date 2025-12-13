#calculate mean diff between img - noimg trials by valence
library(dplyr)
library(tidyr)
between_clean_final <- read.csv("data/processed/between_clean_final.csv")

subj_diffs <- between_clean_final %>%
  group_by(Subject, valence, presence) %>%
  summarise(mean_RT = mean(RT, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = presence, values_from = mean_RT) %>%
  mutate(mean_diff = img - noimg) %>%
  select(Subject, valence, mean_diff)   # keep only keys + diff

# 2) Join this back onto the trial-level data
subj_diffs <- between_clean_final %>%
  left_join(subj_diffs, by = c("Subject", "valence"))

write.csv(subj_diffs, "data/processed/subj_diffs_meanimg_minus_meannoimg.csv", row.names = FALSE)
View(subj_diffs)

