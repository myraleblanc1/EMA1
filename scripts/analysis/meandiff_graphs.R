
library(ggplot2)
library(tidyverse)

mean_diff_df <- read.csv("data/processed/subj_diffs_meanimg_minus_meannoimg.csv")

ggplot(mean_diff_df, aes(x = PPIR40, y = mean_diff, color = RewardCond)) +
  geom_smooth(method = "lm", size = 1.3) +
  facet_wrap(~ ControlCond) +
  labs(
    x = "PPIR40 Score",
    y = "Mean Diff",
    color = "Reward Condition",
    title = "PPIR40 × Reward × Control Interaction on Reaction Time"
  ) +
  theme_minimal(base_size = 14)

#just images
dat_img <- between_clean_final %>%
  filter(presence == "img", !is.na(RT)) %>%
  mutate(
    RewardCond  = factor(RewardCond),
    ControlCond = factor(ControlCond),
    PPIR40      = as.numeric(as.character(PPIR40)),
    PPIR40_c    = as.numeric(scale(PPIR40, center = TRUE, scale = FALSE))
  ) %>%
  filter(!is.na(PPIR40_c))





ggplot(dat_img, aes(x = PPIR40_c, y = RT, color = RewardCond)) +
  geom_smooth(method = "lm", size = 1.3) +
  facet_wrap(~ ControlCond) +
  labs(
    x = "Centered PPIR40 Score",
    y = "Reaction Time (ms)",
    color = "Reward Condition",
    title = "PPIR40 × Reward × Control Interaction on Reaction Time"
  ) +
  theme_minimal(base_size = 14)



