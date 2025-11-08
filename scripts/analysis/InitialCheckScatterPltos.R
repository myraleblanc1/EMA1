library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

# 1) Put all RT metrics into long form
rt_cols <- c(
  "overall_mean_RT",
  "mean_RT_neg_img", "mean_RT_neg_noimg",
  "mean_RT_neu_img", "mean_RT_neu_noimg",
  "mean_RT_pos_img", "mean_RT_pos_noimg"
)


long_rt <- between_wide %>%
  select(Subject, PPIR40, all_of(rt_cols)) %>%
  pivot_longer(cols = all_of(rt_cols), names_to = "metric", values_to = "RT") %>%
  mutate(metric = recode(
    metric,
    overall_mean_RT   = "Overall mean RT",
    mean_RT_neg_img   = "Neg + Image",
    mean_RT_neg_noimg = "Neg + No Image",
    mean_RT_neu_img   = "Neu + Image",
    mean_RT_neu_noimg = "Neu + No Image",
    mean_RT_pos_img   = "Pos + Image",
    mean_RT_pos_noimg = "Pos + No Image"
  ))

# 2) Plot: points + linear fit + Pearson r/p in each facet
p <- ggplot(long_rt, aes(x = PPIR40, y = RT)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  # If ggpubr is available, this adds r and p automatically:
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 3) +
  facet_wrap(~ metric, scales = "free_y") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Psychopathy (PPI-R-40) vs Reaction Time",
    x = "PPI-R-40 Total",
    y = "Mean RT (ms)"
  )
p

# 3) (Optional) Save to file
# ggsave("ppi_vs_rt_facets.png", p, width = 10, height = 8, dpi = 300)
