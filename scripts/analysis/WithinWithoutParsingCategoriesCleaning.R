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
clean_accEMA_3allMAD <- rt_long_no0 |>
  filter(!is.na(RT)) |>
  group_by(Subject) %>%
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
allMADtrim_report <- rt_long |>
  group_by(Subject, RewardCond, ControlCond, img_cat) |>
  summarize(n_pre = sum(!is.na(RT)), .groups = "drop") |>
  left_join(
    clean_accEMA_3allMAD |>
      group_by(Subject, RewardCond, ControlCond, img_cat) |>
      summarize(n_post = n(), .groups = "drop"),
    by = c("Subject","RewardCond","ControlCond","img_cat")
  ) %>%
  mutate(n_post = coalesce(n_post, 0L),
         trimmed = n_pre - n_post,
         pct_trim = if_else(n_pre > 0, 100 * trimmed / n_pre, NA_real_))
#Inspect what was trimmed
View(allMADtrim_report)
#---------------------------------------------------------
#Summarize trimming by img_cat
allMADtrim_by_category <- allMADtrim_report %>%
  group_by(img_cat) %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  ) %>%
  arrange(desc(pct_trim))
View(allMADtrim_by_category)
#---------------------------------------------------------
#Summarize trimming by Subject
allMADtrim_by_subject <- allMADtrim_report %>%
  group_by(Subject) %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  ) %>%
  arrange(desc(pct_trim))
View(allMADtrim_by_subject)
write.csv(allMADtrim_by_subject, "data/processed/allMADtrim_by_subject.csv", row.names = FALSE)
#---------------------------------------------------------
#allMAD trim overall
allMADtrim_overall <- allMADtrim_report %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  )
View(allMADtrim_overall)
###################################################
###################################################
####################################################
###########################################################
###################     allSD     ###########################
###########################################################
#---------------------------------------------------------
# +-3 allSD trimming within Subject × RewardCond × ControlCond × img_cat
clean_accEMA_3allSD <- rt_long_no0 |>
  filter(!is.na(RT)) |>
  group_by(Subject) %>%
  mutate(
    rt_mu = mean(RT, na.rm = TRUE),
    rt_sd = sd(RT, na.rm = TRUE),
    keep = RT >= (rt_mu - 3 * rt_sd) & RT <= (rt_mu + 3 * rt_sd)
  ) %>%
  ungroup() %>%
  filter(keep) %>%
  select(-rt_mu, -rt_sd, -keep)
#---------------------------------------------------------
#check how much was trimmed
allSDtrim_report <- rt_long |>
  group_by(Subject, RewardCond, ControlCond, img_cat) |>
  summarize(n_pre = sum(!is.na(RT)), .groups = "drop") |>
  left_join(
    clean_accEMA_3allSD |>
      group_by(Subject, RewardCond, ControlCond, img_cat) |>
      summarize(n_post = n(), .groups = "drop"),
    by = c("Subject","RewardCond","ControlCond","img_cat")
  ) %>%
  mutate(n_post = coalesce(n_post, 0L),
         trimmed = n_pre - n_post,
         pct_trim = if_else(n_pre > 0, 100 * trimmed / n_pre, NA_real_))
#Inspect what was trimmed
View(allSDtrim_report)
#---------------------------------------------------------
#allSD trim by img_cat
allSDtrim_by_category <- allSDtrim_report %>%
  group_by(img_cat) %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  ) %>%
  arrange(desc(pct_trim))
View(allSDtrim_by_category)
#---------------------------------------------------------
#allSD trim by subject
allSDtrim_by_subject <- allSDtrim_report %>%
  group_by(Subject) %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  ) %>%
  arrange(desc(pct_trim))
View(allSDtrim_by_subject)
write.csv(allSDtrim_by_subject, "data/processed/allSDtrim_by_subject.csv", row.names = FALSE)
#---------------------------------------------------------
#allSD trim overall
allSDtrim_overall <- allSDtrim_report %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  )
View(allSDtrim_overall)
#-----------------------------------------------------------