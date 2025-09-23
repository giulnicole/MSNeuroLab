############################################################
# Script Plot: Mosaic plot
# Author: Giulia Nicole Baldrighi
# Purpose: Create a publication-ready mosaic plot showing
#          the proportion of categories across groups.
# Input:
#   - A 2x2 matrix `mat` with group (rows) and category (columns)
# Output:
#   - A mosaic plot with percentage annotations
############################################################

# ---------------------- Description ----------------------
# Mosaic plots are used to visualize contingency tables. 
# The size of each tile represents the proportion of observations 
# in each group-category combination, allowing for quick comparison 
# of relative frequencies.
# ----------------------------------------------------------

# ---------------------- Example data ----------------------
mat <- matrix(c(73, 27, 23, 77),
              nrow = 2,
              byrow = TRUE,
              dimnames = list(
                Group = c("PF%< 2.12", "PF% >= 2.12"),
                Category = c("Low", "High")
              ))

# ---------------------- Base mosaic plot ----------------------
mp <- mosaicplot(
  mat,
  color = c("#FF9800", "#03A9F4"),  # Orange & Blue
  main = "Distribution of Phenotypes by Ventricular PF%",
  xlab = "Ventricular PF% Group",
  ylab = "Phenotype",
  cex.axis = 1.2,
  cex.lab = 1.3,
  cex.main = 1.5,
  las = 1,       # horizontal labels on y-axis
  border = "white"
)

# ---------------------- Add percentages ----------------------
# Convert counts to percentages
total <- sum(mat)
percentages <- round((mat / total) * 100, 1)

# Get coordinates for annotations
# These values depend on the number of rows and columns.
x_coords <- rep(c(0.30, 0.75), times = nrow(mat))
y_coords <- c(0.70, 0.85, 0.15, 0.30)

# Flatten the matrix into a vector to match coordinates
percentage_labels <- paste0(as.vector(percentages), "%")

# Overlay text
text(
  x = x_coords,
  y = y_coords,
  labels = percentage_labels,
  cex = 1.2,
  font = 2,
  col = "black"
)


# ---------------------- Optional: save plot ----------------------
# ggsave("mosaic_plot.png", width = 9, height = 6, dpi = 300)