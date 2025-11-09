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
#------------------------------------------------------------------------------
#Remove trials with RT <= 200ms
rt_long_no200 <- rt_long |>
  mutate(RT = if_else(RT <= 200, NA_real_, RT))

#---------------------------------------------------------
#check how much was trimmed
no200trim_report <- rt_long |>
  group_by(Subject, RewardCond, ControlCond, img_cat) |>
  summarise(n_pre = sum(!is.na(RT)), .groups = "drop") |>
  left_join(
    rt_long_no200 |>
      filter(!is.na(RT)) |>   # <— add this
      group_by(Subject, RewardCond, ControlCond, img_cat) |>
      summarise(n_post = n(), .groups = "drop"),
    by = c("Subject","RewardCond","ControlCond","img_cat")
  ) |>
  mutate(
    n_post = coalesce(n_post, 0L),
    trimmed = n_pre - n_post,
    pct_trim = if_else(n_pre > 0, 100 * trimmed / n_pre, NA_real_)
  )

#Inspect what was trimmed
View(no200trim_report)
#---------------------------------------------------------
#200 trim by img_cat
no200trim_by_category <- no200trim_report %>%
  group_by(img_cat) %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  ) %>%
  arrange(desc(pct_trim))
View(no200trim_by_category)
#---------------------------------------------------------
#200 trim by subject
no200trim_by_subject <- no200trim_report %>%
  group_by(Subject) %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  ) %>%
  arrange(desc(pct_trim))
View(no200trim_by_subject)

no200trim_with_ppi <- no200trim_by_subject %>%
  mutate(Subject = as.numeric(as.character(Subject))) %>%   # convert factor → numeric
  left_join(
    between_wide %>%
      mutate(Subject = as.numeric(Subject)) %>%             # ensure same type
      select(Subject, PPIR40),
    by = "Subject"
  )

View(no200trim_with_ppi)

# Save to CSV
write.csv(no200trim_with_ppi, "data/processed/no200trim_by_subject_ppi.csv", row.names = FALSE)

#---------------------------------------------------------
#200 trim overall
no200trim_overall <- SDtrim_report %>%
  summarize(
    n_pre = sum(n_pre, na.rm = TRUE),
    n_post = sum(n_post, na.rm = TRUE),
    trimmed = n_pre - n_post,
    pct_trim = 100 * trimmed / n_pre
  )
View(no200trim_overall)
#-----------------------------------------------------------
#########################################################
###########################     SD    ##########################################
#######################################################################
#---------------------------------------------------------
# +-3 SD trimming within Subject × RewardCond × ControlCond × img_cat
clean_accEMA_3SD <- rt_long_no200 |>
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
SDtrim_report <- rt_long_no200 |>
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

#------------------------------------------------------------

write.csv(clean_accEMA_3SD, "data/processed/within_sub_clean.csv", row.names = FALSE)
