library(ggplot2)
library(dplyr)

mean_diff_df <- read.csv("data/processed/subj_diffs_meanimg_minus_meannoimg.csv")

mean_diff_subject <- mean_diff_df %>%
  group_by(Subject) %>%
  summarise(
    mean_diff = mean(mean_diff, na.rm = TRUE),
    PPIR40    = first(PPIR40),
    RewardCond = first(RewardCond),
    ControlCond = first(ControlCond)
  )
ggplot(mean_diff_subject, 
       aes(x = PPIR40, y = mean_diff, color = RewardCond)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal(base_size = 14) +
  labs(
    title = "PPIR40 vs Mean Diff (img - noimg), One Row per Subject",
    x = "PPIR40",
    y = "Mean Difference (img − noimg)",
    color = "Reward Condition"
  )

ggplot(mean_diff_subject, 
       aes(x = PPIR40,
           y = mean_diff,
           color = RewardCond,
           linetype = ControlCond)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal(base_size = 14) +
  labs(
    title = "PPIR40 vs. Mean Diff (img - noimg)\nBy Reward and Control Conditions",
    x = "PPIR40",
    y = "Mean Difference (img − noimg)",
    color = "Reward Condition",
    linetype = "Control Condition"
  )

library(dplyr)
library(tidyr)
library(ggplot2)

mean_diff_by_valence <- mean_diff_df %>%
  group_by(Subject, valence) %>%
  summarise(
    mean_diff   = mean(mean_diff, na.rm = TRUE),
    PPIR40      = first(PPIR40),
    RewardCond  = first(RewardCond),
    ControlCond = first(ControlCond),
    .groups = "drop"
  )

# (optional) make valence a nicely ordered factor
mean_diff_by_valence <- mean_diff_df %>%
  mutate(valence = factor(valence,
                          levels = c("neg", "neu", "pos"),
                          labels = c("Negative", "Neutral", "Positive")))

ggplot(mean_diff_by_valence,
       aes(x = PPIR40,
           y = mean_diff,
           color = RewardCond,
           linetype = ControlCond)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ valence) +   # 👈 separate panels: Negative / Neutral / Positive
  theme_minimal(base_size = 14) +
  labs(
    title = "PPIR40 vs Mean Diff (img - noimg)\nSeparated by Valence",
    x = "PPIR40",
    y = "Mean Difference (img − noimg)",
    color = "Reward Condition",
    linetype = "Control Condition"
  )

