library(dplyr)
library(afex)
library(car)

# 1. Start from your cleaned dataset
dat <- between_clean_final

# 2. Keep only image trials and center PPIR40
dat_img <- dat %>%
  filter(presence == "img", !is.na(RT)) %>%
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
  factorize = FALSE,     # keep PPIR40 numeric
  type = 3
)

# 5. View results
aov_img$anova_table
nice(aov_img)


#-------------------------------------------------------------------------
library(dplyr)
library(afex)
library(car)

# 1) Start from your final dataset (all trials kept)
dat <- between_clean_final

# 2) Coerce types and center PPIR40
dat_all <- dat %>%
  filter(!is.na(RT)) %>%
  mutate(
    RewardCond  = factor(RewardCond),
    ControlCond = factor(ControlCond),
    valence     = factor(valence,  levels = c("neg","pos","neu")),
    presence    = factor(presence, levels = c("img","noimg")),
    PPIR40      = as.numeric(as.character(PPIR40)),
    PPIR40_c    = as.numeric(scale(PPIR40, center = TRUE, scale = FALSE))
  ) %>%
  filter(!is.na(PPIR40_c))

# 3) Collapse to Subject × valence × presence means (so one row per within-cell)
cell_means <- dat_all %>%
  group_by(Subject, RewardCond, ControlCond, valence, presence, PPIR40_c) %>%
  summarise(RT = mean(RT, na.rm = TRUE), .groups = "drop")

# 4) Mixed ANCOVA with PPIR40 interactions (between & within)
#    - Within factors: valence, presence
#    - Between factors: RewardCond, ControlCond
#    - Covariate: PPIR40_c (between-subject) with interactions
aov_all <- aov_car(
  RT ~ PPIR40_c * (RewardCond * ControlCond * valence * presence) +
    Error(Subject/(valence*presence)),
  data       = cell_means,
  factorize  = FALSE,   # keep numeric covariate numeric
  type       = 3
)

# 5) View results (PPIR40_c and all its interactions will be listed)
aov_all$anova_table
nice(aov_all)
#-------------------------------------------------------------------------
dat_all2 <- dat %>%
  filter(!is.na(RT)) %>%
  mutate(
    RewardCond  = factor(RewardCond),
    ControlCond = factor(ControlCond),
    img_cat = factor(
      img_cat,
      levels = c("neg_img","pos_img","neu_img","neg_noimg","pos_noimg","neu_noimg")
    ),
    PPIR40   = as.numeric(as.character(PPIR40)),
    PPIR40_c = as.numeric(scale(PPIR40, center = TRUE, scale = FALSE))
  ) %>%
  filter(!is.na(PPIR40_c))

cell_means6 <- dat_all2 %>%
  group_by(Subject, RewardCond, ControlCond, img_cat, PPIR40_c) %>%
  summarise(RT = mean(RT, na.rm = TRUE), .groups = "drop")

aov_all6 <- aov_car(
  RT ~ PPIR40_c * (RewardCond * ControlCond * img_cat) +
    Error(Subject/img_cat),
  data       = cell_means6,
  factorize  = FALSE,
  type       = 3
)

aov_all6$anova_table
nice(aov_all6)
#-------------------------------------------------------------------------
library(dplyr)
library(afex)
library(car)

# 1) Start from your final dataset
dat <- between_clean_final

# 2) Keep ONLY no-image trials; coerce types; center PPIR40
dat_noimg <- dat %>%
  filter(presence == "noimg", !is.na(RT)) %>%
  mutate(
    RewardCond  = factor(RewardCond),
    ControlCond = factor(ControlCond),
    valence     = factor(valence, levels = c("neg","pos","neu")),
    PPIR40      = as.numeric(as.character(PPIR40)),
    PPIR40_c    = as.numeric(scale(PPIR40, center = TRUE, scale = FALSE))
  ) %>%
  filter(!is.na(PPIR40_c))

# 3) Collapse to Subject × valence means (one row per within-cell)
cell_means_noimg <- dat_noimg %>%
  group_by(Subject, RewardCond, ControlCond, valence, PPIR40_c) %>%
  summarise(RT = mean(RT, na.rm = TRUE), .groups = "drop")

# 4) Mixed ANCOVA with PPIR40 interactions
#    - Within: valence
#    - Between: RewardCond, ControlCond
#    - Covariate: PPIR40_c (between-subject), with interactions
aov_noimg <- aov_car(
  RT ~ PPIR40_c * (RewardCond * ControlCond * valence) +
    Error(Subject/valence),
  data       = cell_means_noimg,
  factorize  = FALSE,   # keep covariate numeric
  type       = 3
)

# 5) View results (includes PPIR40_c and its interactions)
aov_noimg$anova_table
nice(aov_noimg)
#-------------------------------------------------------------------------
library(dplyr)
library(afex)

# 1) Start from your final dataset
dat <- between_clean_final

# 2) Keep all trials; coerce types; center PPIR40
dat_all <- dat %>%
  filter(!is.na(RT)) %>%
  mutate(
    RewardCond  = factor(RewardCond),
    ControlCond = factor(ControlCond),
    valence     = factor(valence, levels = c("neg","pos","neu")),
    PPIR40      = as.numeric(as.character(PPIR40)),
    PPIR40_c    = as.numeric(scale(PPIR40, center = TRUE, scale = FALSE))
  ) %>%
  filter(!is.na(PPIR40_c))

# 3) Collapse presence (img/noimg) → Subject × valence means
cell_means <- dat_all %>%
  group_by(Subject, RewardCond, ControlCond, valence, PPIR40_c) %>%
  summarise(RT = mean(RT, na.rm = TRUE), .groups = "drop")

# 4) Mixed ANCOVA: within = valence; between = RewardCond, ControlCond; covariate = PPIR40
aov_val_only <- aov_ez(
  id         = "Subject",
  dv         = "RT",
  data       = cell_means,
  between    = c("RewardCond","ControlCond"),
  within     = "valence",
  covariate  = "PPIR40_c",   # between-subject covariate
  factorize  = FALSE,        # required so PPIR40_c stays numeric
  type       = 3
)

# 5) Results
aov_val_only$anova_table
nice(aov_val_only)
library(car)

aov_val_only_int <- aov_car(
  RT ~ PPIR40_c * (RewardCond * ControlCond * valence) + Error(Subject/valence),
  data      = cell_means,
  factorize = FALSE,
  type      = 3
)

aov_val_only_int$anova_table
nice(aov_val_only_int)
