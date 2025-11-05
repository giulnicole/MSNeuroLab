

library(ggplot2)
library(ggpubr)
# Compute p-value manually
p <- t.test(value ~ group, data = df)$p.value
p_label <- ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

ggplot(df, aes(x = group, y = value, fill = group)) +
  geom_boxplot() +
  annotate("text", x = 1.5, y = max(df$value) + 0.5, label = paste0("p: ", p_label), size = 5) +
  theme_light() +
  scale_fill_manual(values = c("Control" = "#1b9e77",
                               "Treatment" = "#d95f02"))+
  labs(title = "Boxplot with formatted p-value", y = "Value", x = "Group")
