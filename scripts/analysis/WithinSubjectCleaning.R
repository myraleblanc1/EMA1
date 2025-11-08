#WITHIN SUBJECT cleanING###################################
library(tidyverse)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(psych)
#---------------------------------------------------------
#Make the data long for cleaning and break down by valence/image presence
rt_long <- accEMA %>%
  pivot_longer(
    cols = matches("^(Neg|Pos|Neu)(Irt|Srt)$", ignore.case = TRUE),
    names_to = c("valence_raw","pres_raw"),
    names_pattern = "^(Neg|Pos|Neu)(Irt|Srt)$",
    values_to = "RT"
  ) %>%
  mutate(
    valence = str_to_lower(valence_raw),                   # neg/pos/neu
    presence = if_else(str_to_lower(pres_raw) == "irt",    # irt = image, srt = no-image
                       "img", "noimg"),
    img_cat = factor(
      paste0(valence, "_", presence),
      levels = c("neg_img","pos_img","neu_img","neg_noimg","pos_noimg","neu_noimg")
    )
  )
#------------------------------------------------------------------------------
#Remove trials with RT = 0
rt_long_no0 <- rt_long |>
  mutate(RT = if_else(RT == 0, NA_real_, RT))

#---------------------------------------------------------
# ±2.5 MAD trimming within Subject × RewardCond × ControlCond × img_cat
clean_accEMA_MAD <- rt_long_no0 |>
  filter(!is.na(RT)) |>
  group_by(Subject, img_cat) %>%
  mutate(
    rt_med = median(RT, na.rm = TRUE),
    rt_mad = mad(RT, constant = 1.4826, na.rm = TRUE),
    keep   = RT >= (rt_med - 3 * rt_mad) & RT <= (rt_med + 3 * rt_mad)
  ) %>%
  ungroup() %>%
  filter(keep) %>%
  select(-rt_med, -rt_mad, -keep)
#---------------------------------------------------------
#check how much was trimmed
MADtrim_report <- rt_long |>
  group_by(Subject, RewardCond, ControlCond, img_cat) |>
  summarize(n_pre = sum(!is.na(RT)), .groups = "drop") |>
  left_join(
    clean_accEMA_3MAD |>
      group_by(Subject, RewardCond, ControlCond, img_cat) |>
      summarize(n_post = n(), .groups = "drop"),
    by = c("Subject","RewardCond","ControlCond","img_cat")
  ) %>%
  mutate(n_post = coalesce(n_post, 0L),
         trimmed = n_pre - n_post,
         pct_trim = if_else(n_pre > 0, 100 * trimmed / n_pre, NA_real_))
#Inspect what was trimmed
View(MADtrim_report)
#---------------------------------------------------------
#Summarize trimming by img_cat
MADtrim_by_category <- MADtrim_report %>%
  group_by(img_cat) %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  ) %>%
  arrange(desc(pct_trim))
View(MADtrim_by_category)
#---------------------------------------------------------
#Summarize trimming by Subject
MADtrim_by_subject <- MADtrim_report %>%
  group_by(Subject) %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  ) %>%
  arrange(desc(pct_trim))
View(MADtrim_by_subject)
write.csv(MADtrim_by_subject, "data/processed/MADtrim_by_subject.csv", row.names = FALSE)
#---------------------------------------------------------
#MAD trim overall
MADtrim_overall <- MADtrim_report %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  )
View(MADtrim_overall)
###################################################
###################################################
####################################################
###########################################################
###################     SD     ###########################
###########################################################
#---------------------------------------------------------
# +-3 SD trimming within Subject × RewardCond × ControlCond × img_cat
clean_accEMA_3SD <- rt_long_no0 |>
  filter(!is.na(RT)) |>
  group_by(Subject, img_cat) %>%
  mutate(
    rt_mu = mean(RT, na.rm = TRUE),
    rt_sd = sd(RT, na.rm = TRUE),
    keep = RT >= (rt_mu - 3 * rt_sd) & RT <= (rt_mu + 3 * rt_sd)
  ) %>%
  ungroup() %>%
  filter(keep) %>%
  select(-rt_mu, -rt_sd, -keep)
write.csv(clean_accEMA_3SD, "data/processed/clean_accEMA_3SD.csv", row.names = FALSE)
#---------------------------------------------------------
#check how much was trimmed
SDtrim_report <- rt_long |>
  group_by(Subject, RewardCond, ControlCond, img_cat) |>
  summarize(n_pre = sum(!is.na(RT)), .groups = "drop") |>
  left_join(
    clean_accEMA_3SD |>
      group_by(Subject, RewardCond, ControlCond, img_cat) |>
      summarize(n_post = n(), .groups = "drop"),
    by = c("Subject","RewardCond","ControlCond","img_cat")
  ) %>%
  mutate(n_post = coalesce(n_post, 0L),
         trimmed = n_pre - n_post,
         pct_trim = if_else(n_pre > 0, 100 * trimmed / n_pre, NA_real_))
#Inspect what was trimmed
View(SDtrim_report)
#---------------------------------------------------------
#SD trim by img_cat
SDtrim_by_category <- SDtrim_report %>%
  group_by(img_cat) %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  ) %>%
  arrange(desc(pct_trim))
View(SDtrim_by_category)
#---------------------------------------------------------
#SD trim by subject
SDtrim_by_subject <- SDtrim_report %>%
  group_by(Subject) %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  ) %>%
  arrange(desc(pct_trim))
View(SDtrim_by_subject)
write.csv(SDtrim_by_subject, "data/processed/SDtrim_by_subject.csv", row.names = FALSE)
#---------------------------------------------------------
#SD trim overall
SDtrim_overall <- SDtrim_report %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  )
View(SDtrim_overall)
#-----------------------------------------------------------
#After MAD
library(tidyverse)
library(psych)

subj_means <- clean_accEMA_MAD |>
  group_by(Subject, ControlCond, img_cat) |>
  summarise(RT = mean(RT, na.rm = TRUE), .groups = "drop")
# use median(RT, na.rm = TRUE) if you prefer robustness

# 2) Rename img_cat to the classic column names used in your screenshot
subj_means <- subj_means %>%
  mutate(rt_var = recode(img_cat,
                         "neg_img"    = "NegIrt",
                         "pos_img"    = "PosIrt",
                         "neu_img"    = "NeuIrt",
                         "neg_noimg"  = "NegSrt",
                         "pos_noimg"  = "PosSrt",
                         "neu_noimg"  = "NeuSrt"
  ))

# 3) Pivot wider safely (now no duplicate warning)
wide_rt <- subj_means |>
  pivot_wider(
    names_from  = rt_var,
    values_from = RT,
    values_fill = NA_real_
  )

#------------------------------------------------------------
#Before MAD
accEMA_subj <- rt_long |>
  group_by(Subject, ControlCond, img_cat) |>
  summarise(RT = mean(RT, na.rm = TRUE), .groups = "drop")
# use median(RT, na.rm = TRUE) if you prefer robustness

# 2) Rename img_cat to the classic column names used in your screenshot
accEMA_subj <- accEMA_subj %>%
  mutate(rt_var = recode(img_cat,
                         "neg_img"    = "NegIrt",
                         "pos_img"    = "PosIrt",
                         "neu_img"    = "NeuIrt",
                         "neg_noimg"  = "NegSrt",
                         "pos_noimg"  = "PosSrt",
                         "neu_noimg"  = "NeuSrt"
  ))

# 3) Pivot wider safely (now no duplicate warning)
accEMA_wide_rt <- accEMA_subj |>
  pivot_wider(
    names_from  = rt_var,
    values_from = RT,
    values_fill = NA_real_
  )
#-------------------------------------------------------------
#Table after MAD cleaning
vars <- c("NegIrt","PosIrt","NeuIrt","NegSrt","PosSrt","NeuSrt")
describeBy(wide_rt[vars], group = wide_rt$ControlCond, digits = 2)

#Table before Mad cleaning
vars <- c("NegIrt","PosIrt","NeuIrt","NegSrt","PosSrt","NeuSrt")
describeBy(accEMA_wide_rt[vars], group = accEMA_wide_rt$ControlCond, digits = 2)




rt_cols <- c("NegIrt", "PosIrt", "NeuIrt", "NegSrt", "PosSrt", "NeuSrt")

describeBy(accEMA[rt_cols], group = accEMA$ControlCond, digits = 2, na.rm = TRUE)
#---------------------------------------------------------
#FINAL DATA SAVE HERE FOR NEXT STEPSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS

#
write.csv(wide_rt, "data/processed/within_sub_clean.csv", row.names = FALSE)
