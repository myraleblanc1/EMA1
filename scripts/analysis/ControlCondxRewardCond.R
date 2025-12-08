#Control Cond and Reward COnd
RThyp2cor_plot_faceted <- ggplot(comprehensive_data, 
                                 aes(x = PPIR40, y = NegI_meanRT, color = RewardCond)) +
  
  # Points with semi-transparency
  geom_point(size = 3, alpha = 0.7) +
  
  # Regression lines with confidence intervals
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  
  # Add correlation statistics
  stat_cor(
    aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
    method = "pearson",
    label.x.npc = "center",
    label.y.npc = "top",
    show.legend = FALSE,
    size = 5  # Smaller text for facet labels
  ) +
  
  # Custom colors and labels
  scale_color_manual(
    values = c("Fixed Reward" = "#FF6F05", 
               "Motivated Reward" = "#016FB9"),
    name = "Reward Condition"
  ) +
  
  # Titles and axis labels
  labs(
    title = "PPIR40 Scores vs Reaction Time (Negative Image Trials)",
    subtitle = "Separated by Reward and Control Conditions",
    x = "PPI-R-40 Score (Psychopathy)",
    y = "Reaction Time (ms)"
  ) +
  
  # Facet by ControlCond
  facet_wrap(~ControlCond) +
  
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold")  # Bold facet labels
  )
RThyp2cor_plot_faceted
ggsave("output/hyp2_RT_faceted_plot.png", RThyp2cor_plot_faceted, 
       width = 12, height = 6, dpi = 300)
#--------------------------------
RThyp2cor_plot_combined <- ggplot(comprehensive_data, 
                                  aes(x = PPIR40, y = NegI_meanRT, 
                                      color = RewardCond, 
                                      shape = ControlCond,
                                      linetype = ControlCond)) +
  
  # Points with semi-transparency
  geom_point(size = 3, alpha = 0.7) +
  
  # Regression lines with confidence intervals
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  
  # Add correlation statistics
  stat_cor(
    aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
    method = "pearson",
    label.x.npc = "center",
    label.y.npc = "top",
    show.legend = FALSE,
    size = 5
  ) +
  
  # Custom aesthetics
  scale_color_manual(
    values = c("Fixed Reward" = "#FF6F05", 
               "Motivated Reward" = "#016FB9"),
    name = "Reward Condition"
  ) +
  scale_shape_discrete(name = "Control Condition") +
  scale_linetype_discrete(name = "Control Condition") +
  
  # Titles and axis labels
  labs(
    title = "PPIR40 Scores vs Reaction Time (Negative Image Trials)",
    x = "PPI-R-40 Score (Psychopathy)",
    y = "Reaction Time (ms)"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
RThyp2cor_plot_combined
ggsave("output/hyp2_RT_combined_plot.png", RThyp2cor_plot_combined, 
       width = 10, height = 6, dpi = 300)
#-------------------------------------------------------------------
#COLOUS
# Create a new combined condition variable for coloring
comprehensive_data <- comprehensive_data %>%
  mutate(Reward_Control = interaction(RewardCond, ControlCond, sep = " + "))

# Define custom colors (expand as needed)
custom_colors <- c(
  "Fixed Reward + Proactive" = "#FF6F05",  # Orange
  "Fixed Reward + Reactive" = "#FFF006",  # Lighter orange
  "Motivated Reward + Proactive" = "#016FB9",  # Blue
  "Motivated Reward + Reactive" = "#64B5F6"   # Lighter blue
)
#------------------------
#Negative

NegRThyp2cor_plot_combined <- ggplot(
  comprehensive_data, 
  aes(
    x = PPIR40, 
    y = NegI_meanRT, 
    color = Reward_Control,  # Color by combined condition
    shape = ControlCond,     # Shape by ControlCond (optional)
    linetype = ControlCond   # Linetype by ControlCond (optional)
  )
) +
  
  # Points with semi-transparency
  geom_point(size = 3, alpha = 0.7) +
  
  # Regression lines with confidence intervals
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  
  # Add correlation statistics (grouped by Reward_Control)
  stat_cor(
    aes(group = Reward_Control),
    method = "pearson",
    label.x.npc = "center",
    label.y.npc = "top",
    show.legend = FALSE,
    size = 5
  ) +
  
  # Custom colors, shapes, and linetypes
  scale_color_manual(
    name = "Reward + Control Condition",
    values = custom_colors  # Use predefined colors
  ) +
  scale_shape_discrete(name = "Control Condition") +
  scale_linetype_discrete(name = "Control Condition") +
  
  # Titles and axis labels
  labs(
    title = "PPIR40 Scores vs Reaction Time (Negative Image Trials)",
    subtitle = "Separated by Reward and Control Conditions",
    x = "PPI-R-40 Score (Psychopathy)",
    y = "Reaction Time (ms)"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
NegRThyp2cor_plot_combined
# Save the plot
ggsave("output/hyp2_RT_combined_colorful.png", RThyp2cor_plot_combined, 
       width = 12, height = 6, dpi = 300)
#0--------------------------------
#---------------------------------
#-
#Positive 
# Create a new combined condition variable for coloring


PosRThyp2cor_plot_combined <- ggplot(
  comprehensive_data, 
  aes(
    x = PPIR40, 
    y = PosI_meanRT, 
    color = Reward_Control,  # Color by combined condition
    shape = ControlCond,     # Shape by ControlCond (optional)
    linetype = ControlCond   # Linetype by ControlCond (optional)
  )
) +
  
  # Points with semi-transparency
  geom_point(size = 3, alpha = 0.7) +
  
  # Regression lines with confidence intervals
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  
  # Add correlation statistics (grouped by Reward_Control)
  stat_cor(
    aes(group = Reward_Control),
    method = "pearson",
    label.x.npc = "center",
    label.y.npc = "top",
    show.legend = FALSE,
    size = 5
  ) +
  
  # Custom colors, shapes, and linetypes
  scale_color_manual(
    name = "Reward + Control Condition",
    values = custom_colors  # Use predefined colors
  ) +
  scale_shape_discrete(name = "Control Condition") +
  scale_linetype_discrete(name = "Control Condition") +
  
  # Titles and axis labels
  labs(
    title = "PPIR40 Scores vs Reaction Time (Positive Image Trials)",
    subtitle = "Separated by Reward and Control Conditions",
    x = "PPI-R-40 Score (Psychopathy)",
    y = "Reaction Time (ms)"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
PosRThyp2cor_plot_combined
#0--------------------------------
#---------------------------------
#-
#Neutral
# Create a new combined condition variable for coloring


NeuRThyp2cor_plot_combined <- ggplot(
  comprehensive_data, 
  aes(
    x = PPIR40, 
    y = NeuI_meanRT, 
    color = Reward_Control,  # Color by combined condition
    shape = ControlCond,     # Shape by ControlCond (optional)
    linetype = ControlCond   # Linetype by ControlCond (optional)
  )
) +
  
  # Points with semi-transparency
  geom_point(size = 3, alpha = 0.7) +
  
  # Regression lines with confidence intervals
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  
  # Add correlation statistics (grouped by Reward_Control)
  stat_cor(
    aes(group = Reward_Control),
    method = "pearson",
    label.x.npc = "center",
    label.y.npc = "top",
    show.legend = FALSE,
    size = 5
  ) +
  
  # Custom colors, shapes, and linetypes
  scale_color_manual(
    name = "Reward + Control Condition",
    values = custom_colors  # Use predefined colors
  ) +
  scale_shape_discrete(name = "Control Condition") +
  scale_linetype_discrete(name = "Control Condition") +
  
  # Titles and axis labels
  labs(
    title = "PPIR40 Scores vs Reaction Time (Neutral Image Trials)",
    subtitle = "Separated by Reward and Control Conditions",
    x = "PPI-R-40 Score (Psychopathy)",
    y = "Reaction Time (ms)"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
NeuRThyp2cor_plot_combined
#0--------------------------------------------
#0--------------------------------
#------------------------
#All Images

ImgRThyp2cor_plot_combined <- ggplot(
  comprehensive_data, 
  aes(
    x = PPIR40, 
    y = Image_meanRT, 
    color = Reward_Control,  # Color by combined condition
    shape = ControlCond,     # Shape by ControlCond (optional)
    linetype = ControlCond   # Linetype by ControlCond (optional)
  )
) +
  
  # Points with semi-transparency
  geom_point(size = 3, alpha = 0.7) +
  
  # Regression lines with confidence intervals
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  
  # Add correlation statistics (grouped by Reward_Control)
  stat_cor(
    aes(group = Reward_Control),
    method = "pearson",
    label.x.npc = "center",
    label.y.npc = "top",
    show.legend = FALSE,
    size = 5
  ) +
  
  # Custom colors, shapes, and linetypes
  scale_color_manual(
    name = "Reward + Control Condition",
    values = custom_colors  # Use predefined colors
  ) +
  scale_shape_discrete(name = "Control Condition") +
  scale_linetype_discrete(name = "Control Condition") +
  
  # Titles and axis labels
  labs(
    title = "PPIR40 Scores vs Reaction Time (Image Trials)",
    subtitle = "Separated by Reward and Control Conditions",
    x = "PPI-R-40 Score (Psychopathy)",
    y = "Reaction Time (ms)"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
ImgRThyp2cor_plot_combined
#---------------------------------
#No Images
#All Images

NoImgRThyp2cor_plot_combined <- ggplot(
  comprehensive_data, 
  aes(
    x = PPIR40, 
    y = NoImage_meanRT, 
    color = Reward_Control,  # Color by combined condition
    shape = ControlCond,     # Shape by ControlCond (optional)
    linetype = ControlCond   # Linetype by ControlCond (optional)
  )
) +
  
  # Points with semi-transparency
  geom_point(size = 3, alpha = 0.7) +
  
  # Regression lines with confidence intervals
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  
  # Add correlation statistics (grouped by Reward_Control)
  stat_cor(
    aes(group = Reward_Control),
    method = "pearson",
    label.x.npc = "center",
    label.y.npc = "top",
    show.legend = FALSE,
    size = 5
  ) +
  
  # Custom colors, shapes, and linetypes
  scale_color_manual(
    name = "Reward + Control Condition",
    values = custom_colors  # Use predefined colors
  ) +
  scale_shape_discrete(name = "Control Condition") +
  scale_linetype_discrete(name = "Control Condition") +
  
  # Titles and axis labels
  labs(
    title = "PPIR40 Scores vs Reaction Time (No Image Trials)",
    subtitle = "Separated by Reward and Control Conditions",
    x = "PPI-R-40 Score (Psychopathy)",
    y = "Reaction Time (ms)"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
NoImgRThyp2cor_plot_combined
#---------------------------------
#ACCURCACY
#----------------------------------------------------------------
#Negative
NegACChyp2cor_plot_combined <- ggplot(
  comprehensive_data, 
  aes(
    x = PPIR40, 
    y = NegI_acc, 
    color = Reward_Control,  # Color by combined condition
    shape = ControlCond,     # Shape by ControlCond (optional)
    linetype = ControlCond   # Linetype by ControlCond (optional)
  )
) +
  
  # Points with semi-transparency
  geom_point(size = 3, alpha = 0.7) +
  
  # Regression lines with confidence intervals
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  
  # Add correlation statistics (grouped by Reward_Control)
  stat_cor(
    aes(group = Reward_Control),
    method = "pearson",
    label.x.npc = "center",
    label.y.npc = "top",
    show.legend = FALSE,
    size = 5
  ) +
  
  # Custom colors, shapes, and linetypes
  scale_color_manual(
    name = "Reward + Control Condition",
    values = custom_colors  # Use predefined colors
  ) +
  scale_shape_discrete(name = "Control Condition") +
  scale_linetype_discrete(name = "Control Condition") +
  
  # Titles and axis labels
  labs(
    title = "PPIR40 Scores vs Accuracy (Negative Image Trials)",
    subtitle = "Separated by Reward and Control Conditions",
    x = "PPI-R-40 Score (Psychopathy)",
    y = "Accuracy"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
NegACChyp2cor_plot_combined

#----------------------------------------
#Positive
# Create a new combined condition variable for coloring


PosACChyp2cor_plot_combined <- ggplot(
  comprehensive_data, 
  aes(
    x = PPIR40, 
    y = PosI_acc, 
    color = Reward_Control,  # Color by combined condition
    shape = ControlCond,     # Shape by ControlCond (optional)
    linetype = ControlCond   # Linetype by ControlCond (optional)
  )
) +
  
  # Points with semi-transparency
  geom_point(size = 3, alpha = 0.7) +
  
  # Regression lines with confidence intervals
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  
  # Add correlation statistics (grouped by Reward_Control)
  stat_cor(
    aes(group = Reward_Control),
    method = "pearson",
    label.x.npc = "center",
    label.y.npc = "top",
    show.legend = FALSE,
    size = 5
  ) +
  
  # Custom colors, shapes, and linetypes
  scale_color_manual(
    name = "Reward + Control Condition",
    values = custom_colors  # Use predefined colors
  ) +
  scale_shape_discrete(name = "Control Condition") +
  scale_linetype_discrete(name = "Control Condition") +
  
  # Titles and axis labels
  labs(
    title = "PPIR40 Scores vs Accuracy (Positive Image Trials)",
    subtitle = "Separated by Reward and Control Conditions",
    x = "PPI-R-40 Score (Psychopathy)",
    y = "Accuracy"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
PosACChyp2cor_plot_combined
#----------------------------------------
#Neutral
# Create a new combined condition variable for coloring


NeuACChyp2cor_plot_combined <- ggplot(
  comprehensive_data, 
  aes(
    x = PPIR40, 
    y = NeuI_acc, 
    color = Reward_Control,  # Color by combined condition
    shape = ControlCond,     # Shape by ControlCond (optional)
    linetype = ControlCond   # Linetype by ControlCond (optional)
  )
) +
  
  # Points with semi-transparency
  geom_point(size = 3, alpha = 0.7) +
  
  # Regression lines with confidence intervals
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  
  # Add correlation statistics (grouped by Reward_Control)
  stat_cor(
    aes(group = Reward_Control),
    method = "pearson",
    label.x.npc = "center",
    label.y.npc = "top",
    show.legend = FALSE,
    size = 5
  ) +
  
  # Custom colors, shapes, and linetypes
  scale_color_manual(
    name = "Reward + Control Condition",
    values = custom_colors  # Use predefined colors
  ) +
  scale_shape_discrete(name = "Control Condition") +
  scale_linetype_discrete(name = "Control Condition") +
  
  # Titles and axis labels
  labs(
    title = "PPIR40 Scores vs Accuracy (Neutral Image Trials)",
    subtitle = "Separated by Reward and Control Conditions",
    x = "PPI-R-40 Score (Psychopathy)",
    y = "Accuracy"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
NeuACChyp2cor_plot_combined
#----------------------------------------
#Images
# Create a new combined condition variable for coloring


ImgACChyp2cor_plot_combined <- ggplot(
  comprehensive_data, 
  aes(
    x = PPIR40, 
    y = Image_acc, 
    color = Reward_Control,  # Color by combined condition
    shape = ControlCond,     # Shape by ControlCond (optional)
    linetype = ControlCond   # Linetype by ControlCond (optional)
  )
) +
  
  # Points with semi-transparency
  geom_point(size = 3, alpha = 0.7) +
  
  # Regression lines with confidence intervals
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  
  # Add correlation statistics (grouped by Reward_Control)
  stat_cor(
    aes(group = Reward_Control),
    method = "pearson",
    label.x.npc = "center",
    label.y.npc = "top",
    show.legend = FALSE,
    size = 5
  ) +
  
  # Custom colors, shapes, and linetypes
  scale_color_manual(
    name = "Reward + Control Condition",
    values = custom_colors  # Use predefined colors
  ) +
  scale_shape_discrete(name = "Control Condition") +
  scale_linetype_discrete(name = "Control Condition") +
  
  # Titles and axis labels
  labs(
    title = "PPIR40 Scores vs Accuracy (Image Trials)",
    subtitle = "Separated by Reward and Control Conditions",
    x = "PPI-R-40 Score (Psychopathy)",
    y = "Accuracy"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
ImgACChyp2cor_plot_combined
#----------------------------------------
#NoImages
# Create a new combined condition variable for coloring


NoImgACChyp2cor_plot_combined <- ggplot(
  comprehensive_data, 
  aes(
    x = PPIR40, 
    y = NoImage_acc, 
    color = Reward_Control,  # Color by combined condition
    shape = ControlCond,     # Shape by ControlCond (optional)
    linetype = ControlCond   # Linetype by ControlCond (optional)
  )
) +
  
  # Points with semi-transparency
  geom_point(size = 3, alpha = 0.7) +
  
  # Regression lines with confidence intervals
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  
  # Add correlation statistics (grouped by Reward_Control)
  stat_cor(
    aes(group = Reward_Control),
    method = "pearson",
    label.x.npc = "center",
    label.y.npc = "top",
    show.legend = FALSE,
    size = 5
  ) +
  
  # Custom colors, shapes, and linetypes
  scale_color_manual(
    name = "Reward + Control Condition",
    values = custom_colors  # Use predefined colors
  ) +
  scale_shape_discrete(name = "Control Condition") +
  scale_linetype_discrete(name = "Control Condition") +
  
  # Titles and axis labels
  labs(
    title = "PPIR40 Scores vs Accuracy (No Image Trials)",
    subtitle = "Separated by Reward and Control Conditions",
    x = "PPI-R-40 Score (Psychopathy)",
    y = "Accuracy"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",  # Stack legends vertically
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
NoImgACChyp2cor_plot_combined

