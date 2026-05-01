plot_df <- data.frame(
  x = as.numeric(df$`2`),
  y = df$y,
  ad_type = df$ad_type
)

plot_df <- plot_df[complete.cases(plot_df), ]

model <- glm(y ~ x, data = plot_df, family = binomial)

new_data <- data.frame(
  x = seq(min(plot_df$x), max(plot_df$x), length.out = 300)
)

pred <- predict(model, newdata = new_data, type = "link", se.fit = TRUE)
new_data$prob <- plogis(pred$fit)
new_data$lower <- plogis(pred$fit - 1.96 * pred$se.fit)
new_data$upper <- plogis(pred$fit + 1.96 * pred$se.fit)

ggplot(plot_df, aes(x = x, y = y)) +
  geom_jitter(aes(color = ad_type), height = 0.06, width = 0, size = 2, alpha = 0.65) +
  geom_ribbon(data = new_data, aes(x = x, ymin = lower, ymax = upper),
              inherit.aes = FALSE, fill = "#F9C74F", alpha = 0.3) +
  geom_line(data = new_data, aes(x = x, y = prob),
            inherit.aes = FALSE, color = "#F9844A", linewidth = 1.4) +
  scale_color_manual(values = c("Non-ad" = "#577590", "Ad" = "#F94144")) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Logistic Regression: Aspect Ratio vs Ad Probability",
    subtitle = "Predicted probability that an image is an advertisement",
    x = "Aspect Ratio",
    y = "Probability of Ad",
    color = "Class"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#3C096C"),
    plot.subtitle = element_text(size = 11, color = "#7B2CBF"),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )