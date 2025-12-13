library(tidyverse)
library(ggplot2)
library(afex)
library(emmeans)

# -------------------------------------------------------------------------
# 1. DATA PREP
# -------------------------------------------------------------------------

between_clean_final <- between_clean_final %>%
  mutate(
    RewardCond  = factor(RewardCond),
    ControlCond = factor(ControlCond),
    PPIR40      = as.numeric(PPIR40),
    PPIR40_c    = scale(PPIR40, center = TRUE, scale = FALSE) |> as.numeric()
  )

cell_means <- between_clean_final


# -------------------------------------------------------------------------
# 2. FIT ANCOVA (FOR EFFECT SIZE & P VALUE)
# -------------------------------------------------------------------------

aov_img <- aov_car(
  RT ~ PPIR40_c * RewardCond * ControlCond + Error(Subject),
  data = cell_means,
  factorize = FALSE,
  type = 3
)

# Extract p-value & generalized eta-squared for annotation
p_val  <- aov_img$anova_table["PPIR40_c:RewardCond:ControlCond", "Pr(>F)"]
ges_val <- aov_img$anova_table["PPIR40_c:RewardCond:ControlCond", "ges"]

p_lab  <- paste0("p = ", format(p_val, digits = 3))
ges_lab <- paste0("gη² = ", format(ges_val, digits = 3))


# -------------------------------------------------------------------------
# 3. FIT LINEAR MODEL FOR PLOTTING PREDICTIONS
# -------------------------------------------------------------------------

lm_img <- lm(
  RT ~ PPIR40_c * RewardCond * ControlCond,
  data = cell_means
)


# -------------------------------------------------------------------------
# 4. SIMPLE SLOPES (POST-HOC TESTS)
# -------------------------------------------------------------------------

# Simple slope of PPIR40_c in each Reward × Control condition
slopes <- emtrends(
  lm_img,
  specs = c("RewardCond", "ControlCond"),
  var = "PPIR40_c"
)

print(slopes)

# Pairwise post-hoc comparisons of slopes
slope_tests <- pairs(slopes)
print(slope_tests)

# Convert slopes to dataframe (easy for annotation)
slopes_df <- as.data.frame(slopes)


# -------------------------------------------------------------------------
# 5. PREDICTED VALUES FOR PLOTTING
# -------------------------------------------------------------------------

newdat <- expand.grid(
  PPIR40_c   = seq(min(cell_means$PPIR40_c, na.rm = TRUE),
                   max(cell_means$PPIR40_c, na.rm = TRUE),
                   length.out = 50),
  RewardCond  = levels(cell_means$RewardCond),
  ControlCond = levels(cell_means$ControlCond)
)

newdat$RT_pred <- predict(lm_img, newdata = newdat)


# -------------------------------------------------------------------------
# 6. INTERACTION PLOT WITH ANNOTATION
# -------------------------------------------------------------------------

# Create slope labels for the plot
slopes_summary <- summary(slopes)

slope_labels <- slopes_summary %>%
  mutate(
    label = paste0(
      "Slope = ", round(PPIR40_c.trend, 3),
      "\np = ", signif(p.value, 3)
    )
  ) %>%
  select(RewardCond, ControlCond, label)


ggplot(newdat, aes(x = PPIR40_c, y = RT_pred, color = RewardCond)) +
  geom_line(size = 1.3) +
  facet_wrap(~ ControlCond) +
  labs(
    x = "Centered PPIR40 Score",
    y = "Predicted Reaction Time (ms)",
    color = "Reward Condition",
    title = "PPIR40 × Reward × Control Interaction on Reaction Time"
  ) +
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = paste(p_lab, ges_lab, sep = "\n"),
    hjust = -0.1, vjust = 1.5,
    size = 5
  ) +
  geom_text(
    data = slope_labels,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.1, vjust = 1.2, color = "black", size = 4
  ) +
  theme_minimal(base_size = 14)

# -------------------------------------------------------------------------
#  END
# -------------------------------------------------------------------------
