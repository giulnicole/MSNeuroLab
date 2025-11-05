############################################################
# Script Plot:Dot-and-whisker plot 
# Author: Giulia Nicole Baldrighi 
# Purpose: Create dot-and-whisker plots of regression results
# Input: 
#   - A dataframe `results_clean` with columns:
#       term, OR, CI_low, CI_high, p_value (optional)
# Output:
#   - A publication-ready plot saved or displayed in R
# Dependencies: dplyr, ggplot2, dotwhisker
############################################################

# ---------------------- Description ----------------------
# Dot-and-whisker plots visually summarize regression results 
# by showing the point estimate (dot) and confidence interval 
# (whiskers) for each predictor. They are useful for quickly 
# comparing effect sizes and significance across multiple 
# variables in a single, clean plot.
# ----------------------------------------------------------

# ---- Load libraries ----
library(dplyr)
library(ggplot2)
library(dotwhisker)
library(forcats)

# ---------------------- Example data ----------------------
 results_clean <- data.frame(
   Predictor = rep(c("Age", "Sex", "BMI", "Smoking"), each = 2),
   OR = c(1.25, 1.30, 0.85, 0.88, 1.10, 1.20, 1.40, 1.35),
   CI_low = c(1.10, 1.15, 0.65, 0.70, 0.90, 1.05, 1.10, 1.15),
  CI_high = c(1.40, 1.50, 1.05, 1.10, 1.30, 1.35, 1.70, 1.55),
  p_value = c(0.01, 0.02, 0.04, 0.03, 0.12, 0.10, 0.001, 0.002),
   model = rep(c("Unadjusted", "Adjusted"), 4)
 )


# ---------------------- Prepare data ----------------------
dw_data <- results_clean %>%
  rename(
    estimate = OR,
    conf.low = CI_low,
    conf.high = CI_high,
    term = Predictor
  ) %>%
  mutate(
    model = factor(model, levels = c("Unadjusted", "Adjusted")),
    term = fct_rev(factor(term)),
    label = sprintf("%.2f", estimate),
    sig = ifelse(p_value < 0.05, "Significant", "Not significant")
  )

# ---------------------- Fancy plot ----------------------
dwplot(dw_data, 
       dot_args = list(size = 3), 
       whisker_args = list(size = 1),
       dodge_size = 0.6) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40", size = 0.8) +
  scale_x_log10() +
  theme_minimal(base_size = 13) +
  labs(
    title = "Odds Ratios with 95% Confidence Intervals",
    subtitle = "Comparison of Unadjusted vs Adjusted Models",
    x = "Odds Ratio (log scale)",
    y = "",
    color = "Model"
  ) +
  scale_color_manual(values = c("Unadjusted" = "#1f77b4", "Adjusted" = "#ff7f0e")) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11)
  ) +
  # Add OR labels near the dots
  geom_text(
    data = dw_data,
    aes(x = estimate, y = term, label = label, color = model),
    hjust = -0.5,
    size = 3.5,
    show.legend = FALSE
  ) +
  coord_cartesian(xlim = c(0.5, max(dw_data$conf.high) * 1.5)) +
  guides(color = guide_legend(override.aes = list(size = 4)))

# ---------------------- Optional: save plot ----------------------
# ggsave("dotwhisker_plot_fancy.png", width = 9, height = 6, dpi = 300)
