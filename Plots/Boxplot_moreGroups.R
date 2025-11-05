

library(ggplot2)
library(ggpubr)

set.seed(123)
df2 <- data.frame(
  group = rep(c("Control", "Treatment1", "Treatment2", "Treatment3"), each = 30),
  value = c(rnorm(30, 5, 1),
            rnorm(30, 6, 1.2),
            rnorm(30, 5.5, 1),
            rnorm(30, 6.5, 1.1))
)


# Define all pairwise comparisons
my_comparisons <- list(
  c("Control", "Treatment1"),
  c("Control", "Treatment2"),
  c("Control", "Treatment3"),
  c("Treatment1", "Treatment2"),
  c("Treatment1", "Treatment3"),
  c("Treatment2", "Treatment3")
)

# Boxplot with all pairwise comparisons and p-values
ggplot(df2, aes(x = group, y = value, fill = group)) +
  geom_boxplot() +
  stat_compare_means(
    comparisons = my_comparisons, 
    method = "t.test", 
    label = "p.format",     # show exact p-values
    hide.ns = TRUE           # hide non-significant p-values if desired
  ) +
  scale_fill_manual(values = c("Control" = "#1b9e77",
                               "Treatment1" = "#d95f02",
                               "Treatment2" = "#7570b3",
                               "Treatment3" = "#e7298a")) +
  theme_light() +
  labs(title = "Boxplot with pairwise comparisons", y = "Value", x = "Group")
