str(clean_accEMA_3SD)
library(dplyr)

# Compute each participant's mean RT by Control × Reward × Image condition
rt_summary <- between_clean_final %>%
  group_by(Subject, PPIR40, ControlCond, RewardCond, img_cat) %>%
  summarize(mean_RT = mean(RT, na.rm = TRUE), .groups = "drop")

ggplot(rt_summary, aes(x = PPIR40, y = mean_RT)) +
  geom_point(aes(color = RewardCond, shape = ControlCond), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
  labs(
    title = "Psychopathy vs Reaction Time by Reward & Control Condition",
    x = "Psychopathy Score (PPI-R-40)",
    y = "Mean Reaction Time (ms)"
  ) +
  theme_minimal(base_size = 14)
rt_sum <- clean_accEMA_3SD %>%
  group_by(Subject, PPIR40, ControlCond, RewardCond, img_cat) %>%
  summarize(mean_RT = mean(RT, na.rm = TRUE),
            acc      = mean(c(NegIacc, NegSacc, NeuIacc, NeuSacc, PosIacc, PosSacc), na.rm = TRUE),
            .groups = "drop")
# install.packages("ggridges")
library(ggridges)
ggplot(rt_sum, aes(x = mean_RT, y = img_cat, fill = img_cat)) +
  geom_density_ridges(scale = 1.2, rel_min_height = 0.01, alpha = .8) +
  facet_wrap(~ ControlCond) +
  labs(x = "Mean RT (ms)", y = NULL, title = "RT Distributions by Image Category and Control") +
  theme_minimal(base_size = 14) + theme(legend.position = "none")




