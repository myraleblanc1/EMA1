#ANCOVA mean diff

mean_diff_df <- read.csv("data/processed/subj_diffs_meanimg_minus_meannoimg.csv")


# 2. Keep only image trials and center PPIR40
dat_img <- mean_diff_df %>%
  mutate(
    RewardCond  = factor(RewardCond),
    ControlCond = factor(ControlCond),
    valence     = factor(valence, levels = c("neg","pos","neu")),
    PPIR40      = as.numeric(as.character(PPIR40)),
    PPIR40_c    = scale(PPIR40, center = TRUE, scale = FALSE)  # centered covariate
  )

# 3. Collapse to Subject × valence means
cell_means <- dat_img %>%
  group_by(Subject, RewardCond, ControlCond, valence, PPIR40_c) %>%
  summarise(RT = mean(RT, na.rm = TRUE), .groups = "drop")

# 4. Run ANCOVA with interactions using aov_car()
#    This lets you explicitly include PPIR40_c * (RewardCond * ControlCond * valence)
aov_img <- aov_car(
  RT ~ PPIR40_c * (RewardCond * ControlCond * valence) + Error(Subject/valence),
  data = cell_means,
  factorize = FALSE,
  type = 3
)

# 5. View results
aov_img$anova_table
nice(aov_img)
