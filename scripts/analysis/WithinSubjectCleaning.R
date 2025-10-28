#WITHIN SUBJECT cleanING###################################
library(tidyverse)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
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
#scatterplot
ggplot(clean_accEMA_MAD, aes(x = ControlCond, y = RT, color = img_cat)) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  labs(
    title = "Reaction Time by Control and Reward Conditions",
    x = "Control Condition",
    y = "Reaction Time (ms)",
    color = "Image Category"
  ) +
  facet_wrap(~ ControlCond + RewardCond, nrow = 1, scales = "fixed") +  # one row!
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
#---------------------------------------------------------
#all cats seperate
ggplot(clean_accEMA_MAD, aes(x = ControlCond, y = RT, color = img_cat)) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 1.8) +
  labs(
    title = "Reaction Time by Control, Reward, and Image Category",
    x = "Control Condition",
    y = "Reaction Time (ms)",
    color = "Image Category"
  ) +
  facet_wrap(~ img_cat + RewardCond + ControlCond, nrow = 1, scales = "fixed") +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.spacing.x = unit(0.5, "lines")
  )


#---------------------------------------------------------
library(tidyverse)



acc_condition_rt_means_after <- accEMA %>%
  group_by(Subject, RewardCond, ControlCond) %>%
  summarise(
    # Mean RT for each condition (accurate trials only)
    Acc_NegI_meanRT = mean(NegIrt, na.rm = TRUE),
    Acc_NegS_meanRT = mean(NegSrt, na.rm = TRUE),
    Acc_NeuI_meanRT = mean(NeuIrt, na.rm = TRUE),
    Acc_NeuS_meanRT = mean(NeuSrt, na.rm = TRUE),
    Acc_PosI_meanRT = mean(PosIrt, na.rm = TRUE),
    Acc_PosS_meanRT = mean(PosSrt, na.rm = TRUE),
    
    # Combined means (accurate trials only)
    Acc_Neg_meanRT = mean(c(NegIrt, NegSrt), na.rm = TRUE),
    Acc_Neu_meanRT = mean(c(NeuIrt, NeuSrt), na.rm = TRUE),
    Acc_Pos_meanRT = mean(c(PosIrt, PosSrt), na.rm = TRUE),
    Acc_All_meanRT = mean(c(NegIrt, NegSrt, NeuIrt, NeuSrt, PosIrt, PosSrt), na.rm = TRUE),
    Image_meanRT = mean(c(NegIrt, NeuIrt, PosIrt), na.rm = TRUE),
    NoImage_meanRT = mean(c(NegSrt, NeuSrt, PosSrt), na.rm = TRUE),
    
    
    .groups = 'drop'
  )
# Save the cleaned means to a CSV file
write_csv(acc_condition_rt_means_after, "data/processed/acc_condition_rt_means_after.csv")
acc_condition_rt_means_after
#---------------------------------------------------------
#BEFORE###################################################
##########################################################
##########################################################
##########################################################

acc_condition_rt_means_before <- accEMA %>%
  group_by(Subject, RewardCond, ControlCond) %>%
  summarise(
    # Mean RT for each condition (accurate trials only)
    Acc_NegI_meanRT = mean(NegIrt, na.rm = TRUE),
    Acc_NegS_meanRT = mean(NegSrt, na.rm = TRUE),
    Acc_NeuI_meanRT = mean(NeuIrt, na.rm = TRUE),
    Acc_NeuS_meanRT = mean(NeuSrt, na.rm = TRUE),
    Acc_PosI_meanRT = mean(PosIrt, na.rm = TRUE),
    Acc_PosS_meanRT = mean(PosSrt, na.rm = TRUE),
    
    # Combined means (accurate trials only)
    Acc_Neg_meanRT = mean(c(NegIrt, NegSrt), na.rm = TRUE),
    Acc_Neu_meanRT = mean(c(NeuIrt, NeuSrt), na.rm = TRUE),
    Acc_Pos_meanRT = mean(c(PosIrt, PosSrt), na.rm = TRUE),
    Acc_All_meanRT = mean(c(NegIrt, NegSrt, NeuIrt, NeuSrt, PosIrt, PosSrt), na.rm = TRUE),
    Image_meanRT = mean(c(NegIrt, NeuIrt, PosIrt), na.rm = TRUE),
    NoImage_meanRT = mean(c(NegSrt, NeuSrt, PosSrt), na.rm = TRUE),
    
    
    .groups = 'drop'
  )

write_csv(acc_condition_rt_means_before, "data/processed/acc_condition_rt_means_before.csv")
acc_condition_rt_means_before

#---------------------------------------------------
rt_long_before <- rt_long %>%
  filter(!is.na(RT))
#plot
rt_sum_before <- rt_long_before %>%
  group_by(Subject, RewardCond, ControlCond, img_cat) %>%
  summarize(
    n      = n(),
    meanRT = mean(RT),
    medRT  = median(RT),
    sdRT   = sd(RT),
    seRT   = sdRT / sqrt(pmax(n, 1)),
    .groups = "drop"
  )

ggplot(rt_sum_before, aes(x = img_cat, y = meanRT, fill = RewardCond)) +
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

