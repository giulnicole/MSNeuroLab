
### Dot-and-Whisker plots of regression results

# results_clean has columns: term, OR, CI_low, CI_high

library(dplyr)
library(ggplot2)
library(dotwhisker)


# Longitudinal data format
# 1 col with name of predictor, 2 col with OR, 3 col with CI_low, 4 col with CI_high, 5 col with p_value
# afetr running univariate (repeated mesures or not)

# add a label in model variable which levels are "adjusted" or "unadjasted" that stands for univariate adjustment
results_clean$model <- "Unadjusted"

# Prepare data
dw_data <- results_clean %>%
  rename(estimate = OR,
         conf.low = CI_low,
         conf.high = CI_high,
         term = Predictor)  # 'term' is the name of the predictor

dw_data$term <- factor(dw_data$term, levels = rev(unique(dw_data$term)))


# Create the dwplot
dwplot(dw_data,
       dot_args = list(size = 2),              # size of the point (dot)
       whisker_args = list(size = 1)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkblue") +
  scale_x_log10() +
  theme_minimal() +
  labs(title = "Odds Ratios from Univariate Logistic Models",
       x = "Odds Ratio (log scale)",
       y = "") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12)) +
  scale_color_manual(values = c("royalblue")) +
  guides(color = "none")

# add OR on the right

# Format OR labels (e.g., "1.23")
dw_data$label <- sprintf("%.2f", dw_data$estimate)

dwplot(dw_data,
       dot_args = list(size = 2.5),
       whisker_args = list(size = 1.5)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkblue") +
  scale_x_log10() +
  theme_minimal() +
  labs(title = "Odds Ratios from univariate logistic models (Treated)",
       x = "Odds Ratio (log scale)",
       y = "") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10)) +
  scale_color_manual(values = c("royalblue")) +
  guides(color = "none") +
  # Add OR labels to the right of each point
  geom_text(data = dw_data,
            aes(x = 0.001, y = term, label = label),
            size = 3.5,
            hjust = 0)




