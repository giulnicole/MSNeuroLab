## Spaghetti plot with gittered points and line (on x axis)

library(dplyr)
library(ggplot2)


# Longitudinal data format
# 1 col with ID, 2 col with Group (Treated, Untreated), 3 col with Time (baseline, follow-up)


# This line serves for the jitter step
set.seed(42)

# Apply jitter only to the plotting x-position, but not change the actual 'Time' variable
df_jittered <- df %>%
  mutate(
    Time_num = as.numeric(factor(Time, levels = c("baseline", "follow-up"))),
    Time_jittered = Time_num + runif(n(), -0.1, 0.1)  # consistent jitter for line + point
  )

# Recalculate group means (no jitter here)
group_means <- df %>%
  group_by(Group, Time) %>%
  summarise(mean_rBRL = mean(rBRL), .groups = "drop")

# Plot using jittered x values for both points and lines
ggplot(df_jittered, aes(x = Time_jittered, y = rBRL, group = ID, color = as.factor(Group))) +
  geom_line(alpha = 0.3) +  # subject lines connecting jittered points
  geom_point(size = 2) +    # jittered points
  geom_line(data = group_means,
            aes(x = as.numeric(factor(Time, levels = c("baseline", "follow-up"))), 
                y = mean_rBRL, group = Group),
            color = "black", size = 1.2) +  # mean lines (not jittered)
  scale_x_continuous(breaks = c(1, 2), labels = c("baseline", "follow-up")) +
  theme_minimal()




