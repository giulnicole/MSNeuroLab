#' @title test_with_levene
#' @description
#' \code{\link{test_with_levene}} Function for comparing statistical comparison between numeric variables, testing for assumptions on variables' distribution within levels of categorical variable 
#'
#' @param data data.frame object with the variables (numeric or categorical) on which performing the statistical comparisons
#' @param group_var categorical variable with two levels (classes) in quotes
#' 
#'
#' @return
#' \describe{
#'  \item{final_table}{a data.frame object with results unlisted (with bindrow function}
#'  }
#'  
#' @examples
#' 
#' # Reading the data
#' data<- MSNeuroLab::data
#' # Using the function
#' test_stat <- test_with_levene(data = data[, c(1:4)],  group_var="sex")
#'
#'
#'  
#'  
#'@export
test_with_levene <- function(data, group_var) {
  
  suppressWarnings({
    require(car)
    require(dplyr)
    require(tibble)
  })
  
  if (!group_var %in% names(data)) stop("group_var must be a column in the data")
  
  results <- list()
  
  for (var in names(data)) {
    if (var == group_var) next
    
    # Only process numeric variables
    if (is.numeric(data[[var]])) {
      
      # Get actual group names
      groups <- na.omit(unique(data[[group_var]]))
      if (length(groups) != 2) next  # Skip if not 2 groups
      
      group1 <- as.character(groups[1])
      group2 <- as.character(groups[2])
      
      # Group-wise values
      x <- data[[var]][data[[group_var]] == group1]
      y <- data[[var]][data[[group_var]] == group2]
      
      # Overall stats
      q1 <- round(quantile(data[[var]], 0.25, na.rm = TRUE), 2)
      q3 <- round(quantile(data[[var]], 0.75, na.rm = TRUE), 2)
      
      # Group 1 stats
      q1_g1 <- round(quantile(x, 0.25, na.rm = TRUE), 2)
      q3_g1 <- round(quantile(x, 0.75, na.rm = TRUE), 2)
      med_g1 <- round(median(x, na.rm = TRUE), 2)
      mean_g1 <- round(mean(x, na.rm = TRUE), 2)
      sd_g1 <- round(sd(x, na.rm = TRUE), 2)
      
      # Group 2 stats
      q1_g2 <- round(quantile(y, 0.25, na.rm = TRUE), 2)
      q3_g2 <- round(quantile(y, 0.75, na.rm = TRUE), 2)
      med_g2 <- round(median(y, na.rm = TRUE), 2)
      mean_g2 <- round(mean(y, na.rm = TRUE), 2)
      sd_g2 <- round(sd(y, na.rm = TRUE), 2)
      
      # Normality
      normality <- sapply(groups, function(g) {
        vals <- data[[var]][data[[group_var]] == g]
        if (length(vals) >= 3) {
          shapiro.test(vals)$p.value
        } else {
          NA
        }
      })
      
      symmetric <- all(normality > 0.05, na.rm = TRUE)
      
      # Levene's test
      lev_test <- leveneTest(data[[var]] ~ as.factor(data[[group_var]]), data = data)
      equal_var <- lev_test$`Pr(>F)`[1] > 0.05
      
      if (symmetric) {
        if (equal_var) {
          test_res <- t.test(x, y, var.equal = TRUE)
          test_used <- "t-test (equal var)"
        } else {
          test_res <- t.test(x, y, var.equal = FALSE)
          test_used <- "Welch's t-test (unequal var)"
        }
      } else {
        test_res <- wilcox.test(x, y)
        test_used <- "Wilcoxon"
      }
      
      # Create formatted columns
      overall_median <- round(median(data[[var]], na.rm = TRUE), 2)
      overall_mean <- round(mean(data[[var]], na.rm = TRUE), 2)
      overall_sd <- round(sd(data[[var]], na.rm = TRUE), 2)
      
      summary_stats <- data.frame(
        Variable = var,
        `Median (Q1-Q3)` = paste0(overall_median, " (", q1, "-", q3, ")"),
        `Mean (SD)` = paste0(overall_mean, " (", overall_sd, ")"),
        Symmetric = ifelse(symmetric, "Yes", "No"),
        Equal_Var = ifelse(equal_var, "Yes", "No"),
        Test = test_used,
        P_value = round(test_res$p.value, 4),
        stringsAsFactors = FALSE
      )
      
      # Add group-specific formatted columns
      summary_stats[[paste0("Median (Q1-Q3)_", group1)]] <- paste0(med_g1, " (", q1_g1, "-", q3_g1, ")")
      summary_stats[[paste0("Mean (SD)_", group1)]] <- paste0(mean_g1, " (", sd_g1, ")")
      summary_stats[[paste0("Median (Q1-Q3)_", group2)]] <- paste0(med_g2, " (", q1_g2, "-", q3_g2, ")")
      summary_stats[[paste0("Mean (SD)_", group2)]] <- paste0(mean_g2, " (", sd_g2, ")")
      
      results[[var]] <- summary_stats
    }
    # Removed the categorical variable processing section entirely
  }
  
  final_table <- bind_rows(results)
  return(final_table)
}