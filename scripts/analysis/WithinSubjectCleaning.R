#WITHIN SUBJECT cleanING###################################
library(tidyverse)
library(tidyr)
library(stringr)
library(ggplot2)
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
#---------------------------------------------------------
# ±2.5 MAD trimming within Subject × RewardCond × ControlCond × img_cat
clean_accEMA_MAD <- rt_long %>%
  filter(!is.na(RT)) %>%
  group_by(Subject, RewardCond, ControlCond, img_cat) %>%
  mutate(
    rt_med = median(RT, na.rm = TRUE),
    rt_mad = mad(RT, constant = 1.4826, na.rm = TRUE),
    keep   = RT >= (rt_med - 2.5 * rt_mad) & RT <= (rt_med + 2.5 * rt_mad)
  ) %>%
  ungroup() %>%
  filter(keep) %>%
  select(-rt_med, -rt_mad, -keep)
#---------------------------------------------------------
#check how much was trimmed
trim_report <- rt_long %>%
  group_by(Subject, RewardCond, ControlCond, img_cat) %>%
  summarize(n_pre = sum(!is.na(RT)), .groups = "drop") %>%
  left_join(
    clean_accEMA_MAD %>%
      group_by(Subject, RewardCond, ControlCond, img_cat) %>%
      summarize(n_post = n(), .groups = "drop"),
    by = c("Subject","RewardCond","ControlCond","img_cat")
  ) %>%
  mutate(n_post = coalesce(n_post, 0L),
         trimmed = n_pre - n_post,
         pct_trim = if_else(n_pre > 0, 100 * trimmed / n_pre, NA_real_))
#Inspect what was trimmed
View(trim_report) 
#---------------------------------------------------------
#plot
rt_sum <- clean_accEMA_MAD %>%
  group_by(Subject, RewardCond, ControlCond, img_cat) %>%
  summarize(
    n      = n(),
    meanRT = mean(RT),
    medRT  = median(RT),
    sdRT   = sd(RT),
    seRT   = sdRT / sqrt(pmax(n, 1)),
    .groups = "drop"
  )

ggplot(rt_sum, aes(x = img_cat, y = meanRT, fill = RewardCond)) +
  # Bars with improved contrast
  geom_col(position = position_dodge(width = 0.8), color = "black", width = 0.7) +
  # Error bars
  geom_errorbar(
    aes(ymin = meanRT - seRT, ymax = meanRT + seRT),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  facet_wrap(~ ControlCond) +
  scale_fill_manual(
    values = c("Fixed Reward" = "#1f78b4", "Motivated Reward" = "#33a02c"),  # blue vs. green
    name = "Reward Condition"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Image Condition",
    y = "Mean Reaction Time (ms)",
    title = "Reaction Times by Image Valence, Reward, and Control Condition"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold", size = 13),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
  )
#---------------------------------------------------------
ggplot(clean_accEMA_MAD, aes(x = img_cat, y = RT, fill = RewardCond)) +
  geom_violin(trim = FALSE, alpha = 0.6, position = position_dodge(width = 0.8)) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.8), outlier.shape = NA) +
  facet_wrap(~ ControlCond) +
  scale_fill_manual(
    values = c("Fixed Reward" = "#1f78b4", "Motivated Reward" = "#33a02c"),
    name = "Reward Condition"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Image Condition",
    y = "Reaction Time (ms)",
    title = "Distribution of Reaction Times by Condition"
  ) +
  theme_light(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold", size = 13),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
  )
#---------------------------------------------------------
#SANITY CHECK
# Removal rates overall & per person
removal_summary <- rt_long %>%
  mutate(tag = "pre") %>%
  bind_rows(clean_accEMA_MAD %>% mutate(tag = "post")) %>%
  group_by(tag) %>% summarise(n = n(), .groups="drop") %>%
  tidyr::pivot_wider(names_from = tag, values_from = n) %>%
  mutate(removed = pre - post,
         removed_pct = 100 * removed / pre)

per_subj_removed <- rt_long %>%
  group_by(Subject) %>% summarise(n_pre = n(), .groups="drop") %>%
  left_join(clean_accEMA_MAD %>% group_by(Subject) %>% summarise(n_post = n(), .groups="drop"),
            by = "Subject") %>%
  mutate(n_post = dplyr::coalesce(n_post, 0L),
         removed = n_pre - n_post,
         removed_pct = 100 * removed / n_pre) %>%
  arrange(desc(removed_pct))

# Skewness before vs after (optional)
library(e1071)
skew_pre  <- skewness(dplyr::pull(dplyr::filter(rt_long), RT), na.rm=TRUE)
skew_post <- skewness(clean_accEMA_MAD$RT, na.rm=TRUE)
skew_pre
skew_post
