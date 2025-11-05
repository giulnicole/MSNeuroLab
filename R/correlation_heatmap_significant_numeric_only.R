generate_correlation_heatmap <- function(data,
                                         vars1,
                                         vars2,
                                         alpha = 0.05,
                                         method = "pearson",
                                         adjust = "none",
                                         group_column = "group", #required for title 
                                         digits = 2,
                                         title = "Correlation Heatmap",
                                         text_size = 3) {
  
  # Ensure all necessary packages are loaded
  required_packages <- c("psych","ggplot2","tidyr","dplyr","tibble")
  
  # Check for missing packages
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  if(length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages), "\n")
    install.packages(missing_packages, repos = "https://cran.r-project.org")
  }
  
  require(psych)
  require(ggplot2)
  require(tidyr)
  require(dplyr)
  require(tibble)
  
  # --- 1. Input Validation --- #
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  if (!is.character(vars1) || !is.character(vars2)) {
    stop("'vars1' and 'vars2' must be character vectors of variable names.")
  }
  if (length(vars1) == 0 || length(vars2) == 0) {
    stop("'vars1' and 'vars2' must not be empty.")
  }
  
  all_vars <- unique(c(vars1, vars2))
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("The following variables are not found in the data: ",
         paste(missing_vars, collapse = ", "))
  }
  
  # Ensure data for correlation is numeric
  data_subset <- data[, all_vars, drop = FALSE]
  if (!all(sapply(data_subset, is.numeric))) {
    non_numeric_vars <- names(data_subset)[!sapply(data_subset, is.numeric)]
    stop("All variables selected for correlation must be numeric. Non-numeric found: ",
         paste(non_numeric_vars, collapse = ", "))
  }
  
  
  # --- 2. Calculate Correlations --- #
  # Using tryCatch to handle potential errors from corr.test (e.g., if too few observations)
  corr_results <- tryCatch({
    psych::corr.test(data_subset, method = method, adjust = adjust, ci = FALSE)
  }, error = function(e) {
    stop("Error in psych::corr.test: ", e$message)
  })
  
  cor_matrix <- corr_results$r
  p_matrix <- corr_results$p
  
  # --- 3. Subset to vars1 vs vars2 --- #
  # Ensure we get a matrix even if vars1 or vars2 has length 1
  cor_sub_matrix <- cor_matrix[vars1, vars2, drop = FALSE]
  p_sub_matrix <- p_matrix[vars1, vars2, drop = FALSE]
  
  # --- 4. Prepare data for ggplot2 --- #
  cor_long <- as.data.frame(as.table(cor_sub_matrix))
  names(cor_long) <- c("Var1", "Var2", "Correlation")
  
  p_long <- as.data.frame(as.table(p_sub_matrix))
  names(p_long) <- c("Var1", "Var2", "P_value")
  
  plot_data <- dplyr::left_join(cor_long, p_long, by = c("Var1", "Var2"))
  
  # Ensure factor levels are in the order of input vars for consistent plotting
  plot_data$Var1 <- factor(plot_data$Var1, levels = vars1)
  plot_data$Var2 <- factor(plot_data$Var2, levels = vars2) # Or rev(vars2) for typical matrix display
  
  # --- 5. Create the heatmap --- #
  gg <- ggplot(plot_data, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(color = "white", linewidth = 0.5) + # Add white lines between tiles
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "tomato",
                         midpoint = 0, limit = c(-1, 1),
                         name = "Correlation") +
    geom_text(aes(label = ifelse(P_value < alpha, sprintf(paste0("%.", digits, "f"), Correlation), "")),
              size = text_size, color = "black") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_blank(), # Remove axis titles if Var1/Var2 are self-explanatory
      panel.grid.major = element_blank(), # Remove major grid lines
      panel.border = element_blank(),    # Remove panel border
      legend.position = "right"
    ) +
    labs(title = paste0(title," (",unique(data[[group_column]]),")")) +
    coord_fixed() # Ensures tiles are square
  
  return(gg)
}