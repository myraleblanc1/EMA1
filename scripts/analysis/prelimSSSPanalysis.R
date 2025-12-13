str(between_clean_final)

library(tidyverse)
library(ggplot2)

# 1. Prepare data ----------------------------------------------------------

between_clean_final <- between_clean_final %>%
  mutate(
    RewardCond  = factor(RewardCond),
    ControlCond = factor(ControlCond),
    PPIR40      = as.numeric(PPIR40),
    PPIR40_c    = scale(PPIR40, center = TRUE, scale = FALSE) |> as.numeric()
  )

# 2. Define cell_means (if no further collapsing is needed) ---------------

cell_means <- between_clean_final


# 3. Linear model for visualization ---------------------------------------

lm_img <- lm(
  RT ~ PPIR40_c * RewardCond * ControlCond,
  data = cell_means
)


# 4. Generate prediction grid ----------------------------------------------

newdat <- expand.grid(
  PPIR40_c   = seq(min(cell_means$PPIR40_c, na.rm = TRUE),
                   max(cell_means$PPIR40_c, na.rm = TRUE),
                   length.out = 50),
  RewardCond  = levels(cell_means$RewardCond),
  ControlCond = levels(cell_means$ControlCond)
)

newdat$RT_pred <- predict(lm_img, newdata = newdat)


# 5. Plot the 3-way interaction ---------------------------------------------

ggplot(newdat, aes(x = PPIR40_c, y = RT_pred,
                   color = RewardCond, linetype = ControlCond)) +
  geom_line(size = 1.3) +
  labs(
    x = "Centered PPIR40 Score",
    y = "Predicted Reaction Time (ms)",
    color = "Reward Condition",
    linetype = "Control Condition",
    title = "PPIR40 × Reward × Control Interaction on Reaction Time"
  ) +
  theme_minimal(base_size = 14)

