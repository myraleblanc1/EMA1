#DATA ANALYSIS
#Descriptive Stats
library(psych)
library(ggplot2)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gt)
library(webshot)
library(broom)
library(effectsize)
#-------------------------------------------------------------------------------
#DEMOGRAPHICS
mean(subject_data$Age, na.rm = TRUE)
range(subject_data$Age, na.rm = TRUE)
sd(subject_data$Age, na.rm = TRUE)
subject_data |> summarise(N = n())
table(subject_data$Gender)
table(subject_data$Ethinicity)
range(na.omit(posterData_clean$FixDur))
range(na.omit(posterData_clean$ITIDur))
subject_data |> distinct(Subject, RewardCond) |> 
  count(RewardCond)
subject_data |> distinct(Subject, ControlCond) |> 
  count(ControlCond)
#-------------------------------------------------------------------------------
#groups all the blocks into dataset
rt_cols <- c("NegIrt", "PosIrt", "NeuIrt", "NegSrt", "PosSrt", "NeuSrt")

describe(EMA[rt_cols], na.rm = TRUE)
#Make poster table 
desc_stats <- psych::describe(EMA[rt_cols], na.rm = TRUE) %>% 
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  select(-trimmed, -mad, -min, -max, -range, -vars, -n) %>%
  mutate(Variable = case_when(
    Variable == "NegIrt" ~ "Negative Image",
    Variable == "NegSrt" ~ "Negative No Image",
    Variable == "PosIrt" ~ "Positive Image",
    Variable == "PosSrt" ~ "Positive No Image",
    Variable == "NeuIrt" ~ "Neutral Image",
    Variable == "NeuSrt" ~ "Neutral No Image",
    TRUE ~ Variable  # keeps any other variables unchanged
  ))

descstatsresptime <- desc_stats %>%
  gt() %>%
  tab_header(
    title = "Descriptive Statistics for EMA Response Times",
  ) %>%
  fmt_number(
    columns = -c(Variable),  # Keep integers for vars and n
    decimals = 2
  ) %>%
  cols_label(
    Variable = "",  # Empty label for bolded variable names
    mean = "Mean",
    sd = "SD",
    median = "Median",
    skew = "Skew",
    kurtosis = "Kurtosis",
    se = "SE"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),          # Bold variable names
      cell_text(align = "left")            # Left-align first column
    ),
    locations = cells_body(columns = Variable)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.width = "100%",
    column_labels.font.size = "90%",
    data_row.padding = px(5)
  )

descstatsresptime
gtsave(descstatsresptime, "output/descstatsresptime.png", vwidth = 600, vheight = 600, zoom = 2)


#desc stats for each block type by cog control cond (proactive/reactive)
describeBy(EMA[rt_cols], 
          group = EMA$ControlCond, 
          na.rm = TRUE,
          mat = FALSE) 
#MAKE POSTER TABLE
ControlCond_stats <- describeBy(EMA[rt_cols], 
                            group = EMA$ControlCond, 
                            na.rm = TRUE,
                            mat = FALSE)

# Convert to tidy data frame
ControlCond_stats <- map_df(ControlCond_stats, ~as.data.frame(.x) %>% 
                     tibble::rownames_to_column("Variable"), 
                   .id = "Group") %>%
  select(-trimmed, -mad, -n, -min, -max, -range) %>%  # Remove unwanted columns
  mutate(Variable = case_when(
    Variable == "NegIrt" ~ "Negative Image",
    Variable == "NegSrt" ~ "Negative No Image",
    Variable == "PosIrt" ~ "Positive Image",
    Variable == "PosSrt" ~ "Positive No Image",
    Variable == "NeuIrt" ~ "Neutral Image",
    Variable == "NeuSrt" ~ "Neutral No Image",
    TRUE ~ Variable
  ))
ControlCond_stats <- ControlCond_stats %>%
  # Ensure Group and Variable columns exist and are character type
  mutate(across(c(Group, Variable), as.character)) %>%
  # Remove any rows with NA in key columns
  filter(!is.na(Group), !is.na(Variable)) %>%
  # Select only needed columns
  select(Group, Variable, mean, sd, median, skew, kurtosis, se)


# Create gt table with grouping
ControlCond_table <- ControlCond_stats %>%
  gt(groupname_col = "Group") %>%
  tab_header(
    title = "Descriptive Statistics by Control Condition"
  ) %>%
  fmt_number(
    columns = -c(Group, Variable),
    decimals = 2
  ) %>%
  cols_label(
    Variable = "",
    mean = "Mean",
    sd = "SD",
    median = "Median",
    skew = "Skew",
    kurtosis = "Kurtosis",
    se = "SE"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),          # Bold variable names
      cell_text(align = "left")            # Left-align first column
    ),
    locations = cells_body(columns = Variable)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", size = "110%"),
    locations = cells_row_groups()
  ) %>%
  tab_options(
    table.width = "100%",
    column_labels.font.size = "90%",
    data_row.padding = px(5)
    )

ControlCond_table

gtsave(ControlCond_table, "output/ControlCondrt_stats.png", 
       vwidth = 800, 
       vheight = 1000,
       zoom = 2)
#-------------------------------------------------------------------------------
#desc stats for each block type by reward cond (fixed/motivated)
describeBy(EMA[rt_cols], 
           group = EMA$RewardCond, 
           na.rm = TRUE,
           mat = FALSE)
#MAKE POSTER--------------------------------------------------------------------
#MAKE POSTER TABLE
RewardCond_stats <- describeBy(EMA[rt_cols], 
                                group = EMA$RewardCond, 
                                na.rm = TRUE,
                                mat = FALSE)

# Convert to tidy data frame
RewardCond_stats <- map_df(RewardCond_stats, ~as.data.frame(.x) %>% 
                              tibble::rownames_to_column("Variable"), 
                            .id = "Group") %>%
  select(-trimmed, -mad, -n, -min, -max, -range) %>%  # Remove unwanted columns
  mutate(Variable = case_when(
    Variable == "NegIrt" ~ "Negative Image",
    Variable == "NegSrt" ~ "Negative No Image",
    Variable == "PosIrt" ~ "Positive Image",
    Variable == "PosSrt" ~ "Positive No Image",
    Variable == "NeuIrt" ~ "Neutral Image",
    Variable == "NeuSrt" ~ "Neutral No Image",
    TRUE ~ Variable
  ))
RewardCond_stats <- RewardCond_stats %>%
  # Ensure Group and Variable columns exist and are character type
  mutate(across(c(Group, Variable), as.character)) %>%
  # Remove any rows with NA in key columns
  filter(!is.na(Group), !is.na(Variable)) %>%
  # Select only needed columns
  select(Group, Variable, mean, sd, median, skew, kurtosis, se)


# Create gt table with grouping
RewardCond_table <- RewardCond_stats %>%
  gt(groupname_col = "Group") %>%
  tab_header(
    title = "Descriptive Statistics by Reward Condition"
  ) %>%
  fmt_number(
    columns = -c(Group, Variable),
    decimals = 2
  ) %>%
  cols_label(
    Variable = "",
    mean = "Mean",
    sd = "SD",
    median = "Median",
    skew = "Skew",
    kurtosis = "Kurtosis",
    se = "SE"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),          # Bold variable names
      cell_text(align = "left")            # Left-align first column
    ),
    locations = cells_body(columns = Variable)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", size = "110%"),
    locations = cells_row_groups()
  ) %>%
  tab_options(
    table.width = "100%",
    column_labels.font.size = "90%",
    data_row.padding = px(5)
  )

RewardCond_table

gtsave(RewardCond_table, "output/RewardCondrt_stats.png", 
       vwidth = 800, 
       vheight = 1000,
       zoom = 2)
#-------------------------------------------------------------------------------
#desc stats for valence of distractor image and presence of image
long_data <- EMA %>%
  pivot_longer(
    cols = c(NegIrt, PosIrt, NeuIrt, NegSrt, PosSrt, NeuSrt),
    names_to = c("Valence", "Task"), 
    names_pattern = "(Neg|Pos|Neu)(Irt|Srt)"
  )

#Desc stats for img vs noimg
describeBy(long_data$value, 
           group = list(long_data$Task), 
           mat = TRUE) %>%
  as_tibble()

#Desc stats by valence
describeBy(long_data$value, 
           group = list(long_data$Valence), 
           mat = TRUE) %>%
  as_tibble()
#-------------------------------------------------------------------------------
#SCATTERPLOTS

rt_long_trials <- EMA %>%
  pivot_longer(
    cols = all_of(rt_cols),  # rt_cols = c("NegIrt", "PosIrt", ...)
    names_to = c("Valence", "Task"), 
    names_pattern = "(Neg|Pos|Neu)(Irt|Srt)",
    values_to = "RT"
  ) %>%
  mutate(Condition = paste(Valence, Task, sep = " "))  # Combine Valence + Task for x-axis

ggplot(rt_long_trials, aes(
  x = Condition,  # Conditions on x-axis
  y = RT,         # Individual RTs on y-axis
  color = Task    # Color by task (Irt/Srt)
)) +
  geom_point(
    position = position_jitter(width = 0.4),  # Avoid overplotting
    alpha = 0.6,
    size = 1
  ) +
  labs(
    x = "Condition",
    y = "Reaction Time (ms)",
    title = "Individual RTs by Trial"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#------------

rt_long <- EMA %>%
  # Reshape to long format
  pivot_longer(
    cols = all_of(rt_cols),  # rt_cols = c("NegIrt", "PosIrt", ...)
    names_to = c("Valence", "Task"), 
    names_pattern = "(Neg|Pos|Neu)(Irt|Srt)",
    values_to = "RT"
  ) %>%
  # Combine Valence + Task for x-axis labels
  mutate(Condition = paste(Valence, Task, sep = " ")) %>%
  # Ensure ControlCond is a factor (for grouping)
  mutate(ControlCond = as.factor(ControlCond))

ggplot(rt_long, aes(
  x = Condition,       # Conditions on x-axis
  y = RT,             # Individual RTs on y-axis
  color = Task,       # Color by task (Irt/Srt)
  shape = Valence     # Shape by valence (Neg/Pos/Neu)
)) +
  # Plot individual points with jitter
  geom_point(
    position = position_jitter(width = 0.2),  # Avoid overplotting
    alpha = 0.6,
    size = 1
  ) +
  # Add mean ± SD for reference 
  stat_summary(
    fun.data = "mean_sdl",
    geom = "pointrange",
    color = "black",
    size = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  # Split by ControlCond
  facet_wrap(~ ControlCond) + 
  # Custom colors and shapes
  scale_color_manual(values = c("Irt" = "#1f77b4", "Srt" = "#ff7f0e")) +
  scale_shape_manual(values = c(16, 15, 18)) +  # 16=circle, 15=square, 18=diamond
  # Labels and theme
  labs(
    x = "Condition",
    y = "Reaction Time (ms)",
    title = "RTs by Condition and Control Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-labels
    legend.position = "top"
  )
#-------------------------------------------------------------------
# Calculate accuracy rates by subject for each cond

accEMA <- EMA %>%
  mutate(across(c(NegIacc, NegSacc, NeuIacc, NeuSacc, PosIacc, PosSacc), 
                ~factor(., levels = c(0, 1), labels = c("Incorrect", "Correct")))) %>%
  group_by(Subject) %>%
  summarize(across(c(NegIacc, NegSacc, NeuIacc, NeuSacc, PosIacc, PosSacc),
                   list(
                     Correct = ~sum(. == "Correct", na.rm = TRUE),
                     Incorrect = ~sum(. == "Incorrect", na.rm = TRUE)
                   )))

#----------------------------------------
#Visualization 

acctable <- accPercEMA %>%
  gt() %>%
  tab_header(
    title = md("**Accuracy Rates by Subject**"),
    subtitle = "Percentage Correct by Condition"
  ) %>%
  fmt_percent(
    columns = ends_with("_acc"),
    decimals = 1
  ) %>%
  cols_label(
    Subject = md("**Subject**"),
    NegIacc_acc = md("**Negative-Image**"),
    NegSacc_acc = md("**Negative-NoImage**"),
    NeuIacc_acc = md("**Neutral-Image**"),
    NeuSacc_acc = md("**Neutral-NoImage**"),
    PosIacc_acc = md("**Positive-Image**"),
    PosSacc_acc = md("**Positive-NoImage**"),
    Overall_acc = md("**Overall**"),
    PPIR40 = md("PPIR-40")
  ) %>%
  data_color(
    columns = ends_with("_acc"),
    colors = scales::col_numeric(
      palette = c("#ff6961", "#f8f9fa", "#77dd77"),
      domain = c(0, 1)
    )
  ) %>%
  tab_options(
    table.width = px(800),
    column_labels.font.weight = "bold",
    table.font.size = 14,
    data_row.padding = px(8)
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
    locations = cells_body(rows = nrow(accPercEMA))
  )
acctable
#output the table as a png
gtsave(acctable, 
       "output/accuracy_rates_highresPPIR.png",
       vwidth = 1200, 
       vheight = 800,
       zoom = 3)

#------------------------------------------------------------------------
#accuracy by condition and subject
library(dplyr)
library(tidyr)

# Calculate accuracy by subject and condition
acc_rates <- EMA %>%
  mutate(across(c(NegIacc, NegSacc, NeuIacc, NeuSacc, PosIacc, PosSacc),
                ~as.numeric(. == 1))) %>% # Convert to 1/0
  group_by(Subject, ControlCond, RewardCond) %>%
  summarize(
    NegI_acc = mean(NegIacc, na.rm = TRUE),
    NegS_acc = mean(NegSacc, na.rm = TRUE),
    NeuI_acc = mean(NeuIacc, na.rm = TRUE),
    NeuS_acc = mean(NeuSacc, na.rm = TRUE),
    PosI_acc = mean(PosIacc, na.rm = TRUE),
    PosS_acc = mean(PosSacc, na.rm = TRUE),
    Overall_acc = mean(c(NegIacc, NegSacc, NeuIacc, NeuSacc, PosIacc, PosSacc), na.rm = TRUE),
    .groups = 'drop'
  )



# Create a combined plot for all conditions

acc_rates %>%
  pivot_longer(cols = ends_with("_acc"), 
               names_to = "Condition", 
               values_to = "Accuracy") %>%
  mutate(Condition = factor(Condition,
                            levels = c("NegI_acc", "NegS_acc", "NeuI_acc", 
                                       "NeuS_acc", "PosI_acc", "PosS_acc",
                                       "Overall_acc"),
                            labels = c("Negative-I", "Negative-S", "Neutral-I",
                                       "Neutral-S", "Positive-I", "Positive-S",
                                       "Overall"))) %>%
  
  ggplot(aes(x = ControlCond, y = Accuracy, fill = RewardCond)) +
  geom_boxplot() +
  facet_wrap(~Condition) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Accuracy Rates by Control and Reward Conditions",
       x = "Control Condition",
       y = "Accuracy Rate",
       fill = "Reward Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#-----------------------------------------------------------
#CORRELATION BETWEEN ACCURACY AND PPIR40 scores

cor_test_results <- map_df(
  names(accPercEMA)[grepl("_acc", names(accPercEMA))], 
  ~{
    # Run correlation test
    test <- cor.test(accPercEMA$PPIR40, accPercEMA[[.x]])
    
    # Calculate effect sizes
    n <- sum(complete.cases(accPercEMA$PPIR40, accPercEMA[[.x]]))
    r <- test$estimate
    d <- 2 * r / sqrt(1 - r^2)  # Cohen's d from correlation
    r2 <- r^2                    # R-squared
    
    # Return tidy results with effect sizes
    tibble(
      Accuracy_Measure = .x,
      estimate = r,
      p.value = test$p.value,
      statistic = test$statistic,
      Cohens_d = d,
      R_squared = r2,
      Effect_Size = interpret_cohens_d(d), # Optional interpretation
      CI_low = test$conf.int[1],
      CI_high = test$conf.int[2]
    )
  }
)

# Format results table
cor_test_results %>%
  select(Accuracy_Measure, estimate, p.value, Cohens_d, R_squared, Effect_Size) %>%
  gt() %>%
  fmt_number(columns = c(estimate, Cohens_d, R_squared), decimals = 3) %>%
  tab_header(
    title = "Correlations with Effect Sizes",
    subtitle = "PPIR40 vs Accuracy Measures"
  ) %>%
  cols_label(
    Accuracy_Measure = "Accuracy Measure",
    estimate = "Pearson's r",
    p.value = "p-value",
    Cohens_d = "Cohen's d",
    R_squared = "R²",
    Effect_Size = "Effect Size"
  )
#----------------------------------------------------------
#Correlation between RT and PPIR40
library(tidyverse)
library(broom)
library(effectsize)
library(gt)
library(patchwork)

# 1. Calculate mean RTs for each original condition
rt_means <- EMA %>%
  group_by(Subject, PPIR40) %>%
  summarize(
    NegIrt_mean = mean(NegIrt, na.rm = TRUE),
    NegSrt_mean = mean(NegSrt, na.rm = TRUE),
    NeuIrt_mean = mean(NeuIrt, na.rm = TRUE),
    NeuSrt_mean = mean(NeuSrt, na.rm = TRUE),
    PosIrt_mean = mean(PosIrt, na.rm = TRUE),
    PosSrt_mean = mean(PosSrt, na.rm = TRUE),
    .groups = 'drop'
  )

# 2. Analyze correlations with PPIR40
rt_results <- tibble(
  Condition = c("Negative_Image", "Negative_NoImage", 
                "Neutral_Image", "Neutral_NoImage",
                "Positive_Image", "Positive_NoImage"),
  RT_Column = c("NegIrt_mean", "NegSrt_mean", 
                "NeuIrt_mean", "NeuSrt_mean",
                "PosIrt_mean", "PosSrt_mean")
) %>%
  rowwise() %>%
  mutate(
    test = list(cor.test(rt_means$PPIR40, rt_means[[RT_Column]])),
    n = sum(complete.cases(rt_means$PPIR40, rt_means[[RT_Column]])),
    tidy_test = list(broom::tidy(test)),
    Mean_RT = mean(rt_means[[RT_Column]], na.rm = TRUE),
    SD_RT = sd(rt_means[[RT_Column]], na.rm = TRUE)
  ) %>%
  unnest(tidy_test) %>%
  mutate(
    Cohens_d = 2 * estimate / sqrt(1 - estimate^2),
    R_squared = estimate^2,
    Effect_Size = interpret_cohens_d(Cohens_d)
  ) %>%
  select(Condition, Mean_RT, SD_RT, Correlation = estimate, p.value, 
         Cohens_d, R_squared, Effect_Size, CI_low = conf.low, CI_high = conf.high)

# 3. Create results table
rt_results %>%
  gt() %>%
  fmt_number(columns = c(Mean_RT, SD_RT, Correlation, Cohens_d, R_squared, CI_low, CI_high), 
             decimals = 2) %>%
  fmt_scientific(columns = p.value, decimals = 3) %>%
  tab_header(
    title = "PPIR40 and Reaction Time Relationships",
    subtitle = "Mean RTs with Correlations and Effect Sizes"
  ) %>%
  cols_label(
    Condition = "Condition",
    Mean_RT = "Mean RT (ms)",
    SD_RT = "SD RT",
    Correlation = "r",
    p.value = "p-value",
    Cohens_d = "Cohen's d",
    R_squared = "R²",
    Effect_Size = "Effect Size",
    CI_low = "CI Low",
    CI_high = "CI High"
  ) %>%
  tab_style(
    style = cell_fill(color = "#FFEEEE"),
    locations = cells_body(columns = p.value, rows = p.value < 0.05)
  )

# 4. Corrected visualization
plot_list <- list(
  "Negative_Image" = ggplot(rt_means, aes(x = PPIR40, y = NegIrt_mean)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "steelblue") +
    labs(title = "Negative Image", y = "RT (ms)", x = "PPIR40") +
    theme_minimal(),
  
  "Negative_NoImage" = ggplot(rt_means, aes(x = PPIR40, y = NegSrt_mean)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "steelblue") +
    labs(title = "Negative No Image", y = "RT (ms)", x = "PPIR40") +
    theme_minimal(),
  
  "Neutral_Image" = ggplot(rt_means, aes(x = PPIR40, y = NeuIrt_mean)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "steelblue") +
    labs(title = "Neutral Image", y = "RT (ms)", x = "PPIR40") +
    theme_minimal(),
  
  "Neutral_NoImage" = ggplot(rt_means, aes(x = PPIR40, y = NeuSrt_mean)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "steelblue") +
    labs(title = "Neutral No Image", y = "RT (ms)", x = "PPIR40") +
    theme_minimal(),
  
  "Positive_Image" = ggplot(rt_means, aes(x = PPIR40, y = PosIrt_mean)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "steelblue") +
    labs(title = "Positive Image", y = "RT (ms)", x = "PPIR40") +
    theme_minimal(),
  
  "Positive_NoImage" = ggplot(rt_means, aes(x = PPIR40, y = PosSrt_mean)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "steelblue") +
    labs(title = "Positive No Image", y = "RT (ms)", x = "PPIR40") +
    theme_minimal()
)

# Combine plots
wrap_plots(plot_list, ncol = 3) +
  plot_annotation(title = "PPIR40-Reaction Time Relationships by Condition")

# Save plot
ggsave("output/ppir40_rt_relationships.png", width = 12, height = 8)
#----------------------------------------------------
#PAIRED T TEST BETWEEN PPIR40 and 

rt_data <- EMA %>%
  select(Subject, 
         NegIrt, NegSrt,  # Negative Image, Negative No Image
         PosIrt, PosSrt) %>%       # Positive Image
  group_by(Subject) %>%
  summarize(
    Neg_Image = mean(NegIrt, na.rm = TRUE),
    Neg_NoImage = mean(NegSrt, na.rm = TRUE),
    Pos_Image = mean(PosIrt, na.rm = TRUE),
    Pos_NoImage = mean(PosSrt, na.rm = TRUE)
  )

# 2. Paired t-test: Negative Image vs. Positive Image
negimg_vs_posimg <- t.test(rt_data$Neg_Image, rt_data$Pos_Image, paired = TRUE)

# 3. Paired t-test: Negative No Image vs. Positive Image
negsimg_vs_posimg <- t.test(rt_data$Neg_NoImage, rt_data$Pos_NoImage, paired = TRUE)

# 4. Calculate effect sizes (Cohen's d for paired samples)
effects <- list(
  "NegImage_vs_PosImage" = cohens_d(
    rt_data$Neg_Image, 
    rt_data$Pos_Image,
    paired = TRUE
  ),
  "NegNoImage_vs_PosImage" = cohens_d(
    rt_data$Neg_NoImage,
    rt_data$Pos_Image,
    paired = TRUE
  )
)

# 5. Create results table
results <- tibble(
  Comparison = c("Negative Image vs Positive Image", 
                 "Negative No Image vs Positive Image"),
  Mean_Diff = c(
    mean(rt_data$Neg_Image - rt_data$Pos_Image, na.rm = TRUE),
    mean(rt_data$Neg_NoImage - rt_data$Pos_Image, na.rm = TRUE)
  ),
  t_statistic = c(negimg_vs_posimg$statistic, negsimg_vs_posimg$statistic),
  df = c(negimg_vs_posimg$parameter, negsimg_vs_posimg$parameter),
  p_value = c(negimg_vs_posimg$p.value, negsimg_vs_posimg$p.value),
  Cohens_d = c(effects[[1]]$Cohens_d, effects[[2]]$Cohens_d),
  CI_low = c(effects[[1]]$CI_low, effects[[2]]$CI_low),
  CI_high = c(effects[[1]]$CI_high, effects[[2]]$CI_high)
)

# 6. Format results
results %>%
  gt() %>%
  fmt_number(columns = c(Mean_Diff, t_statistic, Cohens_d, CI_low, CI_high), decimals = 2) %>%
  fmt_scientific(columns = p_value, decimals = 3) %>%
  tab_header(
    title = "Paired Comparisons of Reaction Times",
    subtitle = "Negative vs Positive Image Conditions"
  ) %>%
  cols_label(
    Comparison = "Comparison",
    Mean_Diff = "Mean Difference (ms)",
    t_statistic = "t-value",
    df = "Degrees of Freedom",
    p_value = "p-value",
    Cohens_d = "Cohen's d",
    CI_low = "CI Low",
    CI_high = "CI High"
  ) %>%
  tab_style(
    style = cell_fill(color = "#FFEEEE"),
    locations = cells_body(columns = p_value, rows = p_value < 0.05)
  )

#0------------------------
#-------------

rt_data <- EMA %>%
  select(Subject, 
         NegIrt, NegSrt,  # Negative Image, Negative No Image
         PosIrt, PosSrt, PPIR40) %>%       # Positive Image
  group_by(Subject) %>%
  summarize(
    Neg_Image = mean(NegIrt, na.rm = TRUE),
    Neg_NoImage = mean(NegSrt, na.rm = TRUE),
    Pos_Image = mean(PosIrt, na.rm = TRUE),
    Pos_NoImage = mean(PosSrt, na.rm = TRUE),
    PPIR40 = first(PPIR40),
    .groups = 'drop'
  )


# Step 1: Create subject-level dataset with unique PPIR40 scores
subject_level_data <- rt_data %>%
  group_by(Subject) %>%
  summarise(
    PPIR40 = first(PPIR40),  # Takes the first PPIR40 value for each subject
    .groups = 'drop'
  ) %>%
  # Step 2: Perform median split at subject level
  mutate(
    psychopathy_group = ifelse(
      PPIR40 >= median(PPIR40, na.rm = TRUE),
      "high", "low"
    ) %>% factor(levels = c("low", "high"))
  )

# Step 3: Merge back with original data
ema_data <- rt_data %>%
  left_join(subject_level_data %>% select(Subject, psychopathy_group),
            by = "Subject")

str(ema_data$psychopathy_group)
#-----
var_pairs <- list(
  c("NegSrt", "PosSrt"),  # First pair
  c("NegSrt", "PosSrt")   # Second pair
)
str(var_pairs)
#---------
print(levels(ema_data$psychopathy_group))

low_group <- ema_data$Neg_Image[ema_data$psychopathy_group == "low"]
high_group <- ema_data$Neg_Image[ema_data$psychopathy_group == "high"]

t_test_result <- t.test(low_group, high_group)
effect_size <- cohen.d(low_group, high_group)

print(t_test_result)
print(effect_size)
#--------------------------------------------------------------------
desc_stats <- ema_data %>%
  group_by(psychopathy_group) %>%
  summarise(
    n = n_distinct(Subject),
    Mean = mean(Neg_Image, na.rm = TRUE),
    SD = sd(Neg_Image, na.rm = TRUE),
    .groups = 'drop'
  )

# 4. Create results table
results_table <- tibble(
  Comparison = "High vs Low Psychopathy (Neg_Image)",
  t = t_test_result$statistic,
  df = t_test_result$parameter,
  p_value = t_test_result$p.value,
  Cohen_d = effect_size$estimate,
  CI_lower = effect_size$conf.int[1],
  CI_upper = effect_size$conf.int[2]
)

# 5. Create gt tables
desc_gt <- desc_stats %>%
  gt() %>%
  tab_header(
    title = "Descriptive Statistics",
    subtitle = "Neg_Image by Psychopathy Group"
  ) %>%
  fmt_number(columns = c(Mean, SD), decimals = 2) %>%
  cols_label(
    psychopathy_group = "Group",
    n = "N",
    Mean = "Mean",
    SD = "SD"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

results_gt <- results_table %>%
  gt() %>%
  tab_header(
    title = "Inferential Statistics",
    subtitle = "T-test Results for Neg_Image"
  ) %>%
  fmt_number(columns = c(t, df, Cohen_d, CI_lower, CI_upper), decimals = 2) %>%
  fmt_scientific(columns = p_value, decimals = 3) %>%
  cols_label(
    Comparison = "Comparison",
    t = "t-value",
    df = "df",
    p_value = "p-value",
    Cohen_d = "Cohen's d",
    CI_lower = "CI Lower",
    CI_upper = "CI Upper"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  cols_merge(
    columns = c(CI_lower, CI_upper),
    pattern = "[{1}, {2}]"
  ) %>%
  cols_label(
    CI_lower = "95% CI"
  )

# 6. Display tables
desc_gt
results_gt
