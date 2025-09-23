############################################################
# Script Plot: Spaghetti plot
# Author: Giulia Nicole Baldrighi
# Purpose: Visualize longitudinal data with jittered spaghetti plot
# Input:
#   df with columns:
#       ID, Group, Time (baseline, follow-up), rBRL (numeric)
############################################################

library(dplyr)
library(ggplot2)

# ---------------------- Example data ----------------------
# set.seed(42) 
 df <- data.frame(
   ID = rep(1:20, each = 2),
   Group = rep(c("Treated", "Untreated"), each = 20),
   Time = rep(c("baseline", "follow-up"), times = 20),
   rBRL = c(rnorm(20, 1, 0.2), rnorm(20, 1.5, 0.2))
 )

# ---------------------- Prepare data ----------------------
set.seed(42)
df_jittered <- df %>%
  mutate(
    Time_num = as.numeric(factor(Time, levels = c("baseline", "follow-up"))),
    Time_jittered = Time_num + runif(n(), -0.08, 0.08) # jitter for clarity
  )

# Group means (without jitter)
group_means <- df %>%
  group_by(Group, Time) %>%
  summarise(mean_rBRL = mean(rBRL), .groups = "drop") %>%
  mutate(Time_num = as.numeric(factor(Time, levels = c("baseline", "follow-up"))))


# ---------------------- Plot ----------------------
ggplot() +
  # Individual subject lines and points
  geom_line(
    data = df_jittered,
    aes(x = Time_jittered, y = rBRL, group = ID, color = Group),
    alpha = 0.3, size = 0.8
  ) +
  geom_point(
    data = df_jittered,
    aes(x = Time_jittered, y = rBRL, color = Group),
    size = 2, alpha = 0.7
  ) +
  
  # Group-level mean lines and points (no ID mapping)
  geom_line(
    data = group_means,
    aes(x = Time_num, y = mean_rBRL, group = Group),
    color = "black", size = 1.2
  ) +
  geom_point(
    data = group_means,
    aes(x = Time_num, y = mean_rBRL),
    shape = 21, fill = "white", color = "black", size = 3, stroke = 1
  ) +
  
  # Axis and colors
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Baseline", "Follow-up")
  ) +
  scale_color_manual(values = c("Treated" = "#1f77b4", "Untreated" = "#ff7f0e")) +
  
  # Titles and theme
  labs(
    title = "Longitudinal change in rBRL by group",
    x = "Time",
    y = "rBRL Value",
    color = "Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "top"
  )


# ---------------------- Optional: save plot ----------------------
# ggsave("spaghetti_plot.png", width = 9, height = 6, dpi = 300)
