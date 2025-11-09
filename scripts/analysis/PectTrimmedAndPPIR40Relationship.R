# Correlation test
# Remove Subject 132
no200trim_no132 <- no200trim_with_ppi %>%
  filter(Subject != 132)

shapiro.test(no200trim_no132$pct_trim)
shapiro.test(no200trim_no132$PPIR40)

# Visual checks
par(mfrow=c(2,2))
hist(no200trim_no132$pct_trim,  breaks=20, main="pct_trim hist")
qqnorm(no200trim_no132$pct_trim); qqline(no200trim_no132$pct_trim)

hist(no200trim_no132$PPIR40,    breaks=20, main="PPIR40 hist")
qqnorm(no200trim_no132$PPIR40); qqline(no200trim_no132$PPIR40)
par(mfrow=c(1,1))
# Run correlation
cor.test(no200trim_no132$pct_trim, no200trim_no132$PPIR40, method = "pearson")

no200model <- lm(pct_trim ~ PPIR40, data = no200trim_no132)
summary(no200model)
# Plot
ggplot(no200trim_no132, aes(x = PPIR40, y = pct_trim)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_y_continuous(limits = c(0, 25), expand = c(0, 0)) +
  labs(
    title = "Correlation Between PPI-R-40 Scores and % Trials Trimmed (Subject 132 Removed)",
    x = "PPI-R-40 Total Score",
    y = "Percentage of Trials Trimmed (%)"
  ) +
  theme_minimal(base_size = 14)
