#Correlations
library(dplyr)
library(tidyr)
#PIVOT WIDER

between_clean_final <- between_clean_final %>%
  mutate(
    acc_trial = case_when(
      img_cat == "neg_img"  ~ as.numeric(NegIacc),
      img_cat == "neg_noimg"~ as.numeric(NegSacc),
      img_cat == "neu_img"  ~ as.numeric(NeuIacc),
      img_cat == "neu_noimg"~ as.numeric(NeuSacc),
      img_cat == "pos_img"  ~ as.numeric(PosIacc),
      img_cat == "pos_noimg"~ as.numeric(PosSacc),
      TRUE ~ NA_real_
    )
  )
between_summary <- between_clean_final %>%
  group_by(Subject, RewardCond, ControlCond) %>%
  mutate(overall_mean_RT = mean(RT, na.rm = TRUE)) %>%
  group_by(Subject, RewardCond, ControlCond, valence, presence) %>%
  summarize(
    mean_RT  = mean(RT, na.rm = TRUE),
    mean_acc = mean(acc_trial, na.rm = TRUE),
    overall_mean_RT = first(overall_mean_RT),
    PPIR40   = first(PPIR40),
    Age      = first(Age),
    Gender   = first(Gender),
    Ethinicity = first(Ethinicity),
    .groups  = "drop"
  )

between_wide <- between_summary %>%
  unite(cond, valence, presence, sep = "_") %>%
  pivot_wider(
    names_from = cond,
    values_from = c(mean_RT, mean_acc),
    names_glue = "{.value}_{cond}"
  )
#---------------------------------------------------------
#add overall RT


cor.test(between_wide$PPIR40, between_wide$overall_mean_RT, method = "pearson")
cor.test(between_wide$PPIR40, between_wide$mean_RT_neg_img, method = "pearson")
cor.test(between_wide$PPIR40, between_wide$mean_RT_neg_noimg, method = "pearson")
cor.test(between_wide$PPIR40, between_wide$mean_RT_neu_img, method = "pearson")
cor.test(between_wide$PPIR40, between_wide$mean_RT_neu_noimg, method = "pearson")
cor.test(between_wide$PPIR40, between_wide$mean_RT_pos_img, method = "pearson")
cor.test(between_wide$PPIR40, between_wide$mean_RT_pos_noimg, method = "pearson")


