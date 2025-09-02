


# Draw the mosaic plot and capture tile coordinates
mp <- mosaicplot(mat,
                 color = c("orange", "lightblue"),
                 main = "",
                 xlab = "Criteria",
                 ylab = "Phenotype",
                 cex.axis = 1, cex.lab = 1, cex.main = 2,
                 las = 1,       # y-axis labels horizontal
                 border = "white")  

# Add percentages
# Convert counts to percentages
total <- sum(mat)
percentages <-  mat

# Calculate positions for text
x_pos <- c(0.30, 0.75, 0.30, 0.75)
y_pos <- c(0.65, 0.85, 0.10, 0.35)

# Overlay percentages inside the tiles
text(x = x_pos, y = y_pos, labels = paste0(percentages, "%"), cex = 1.2, col = "black")
