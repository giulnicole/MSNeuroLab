#' @title test_with_levene
#' @description
#' \code{\link{test_with_levene}} Function for comparing statistical comparison between numeric variables, testing for assumptions on variables' distribution within levels of categorical variable 
#'
#' @param data data.frame object with the variables (numeric or categorical) on which performing the statistical comparisons
#' @param group_var categorical variable with two levels (classes) in quotes
#' 
#' @name test_with_levene
#'
#' @return
#'  \item{final_table}{a data.frame object with results unlisted (with bindrow function}
#'  
#'  
#' @examples
#' \dontrun{
#' 
#' library(readxl)
#' 
#' # Reading the data
#' data<- read_excel("Data/examination_dataset1.xlsx")
#'  
#' # Using the function
#' test_stat <- test_with_levene(data = data[, c(1:4)],  group_var="sex")
#'
#'}
#'                           
#'
#'
#'  
#'  
#'  @export
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
      
      summary_stats <- data.frame(
        Variable = var,
        Median = round(median(data[[var]], na.rm = TRUE), 2),
        Q1 = q1,
        Q3 = q3,
        Mean = round(mean(data[[var]], na.rm = TRUE), 2),
        SD = round(sd(data[[var]], na.rm = TRUE), 2),
        Symmetric = ifelse(symmetric, "Yes", "No"),
        Equal_Var = ifelse(equal_var, "Yes", "No"),
        Test = test_used,
        P_value = round(test_res$p.value, 4),
        stringsAsFactors = FALSE
      )
      
      # Add group-specific columns with actual names
      summary_stats[[paste0("Median_", group1)]] <- med_g1
      summary_stats[[paste0("Q1_", group1)]] <- q1_g1
      summary_stats[[paste0("Q3_", group1)]] <- q3_g1
      summary_stats[[paste0("Mean_", group1)]] <- mean_g1
      summary_stats[[paste0("SD_", group1)]] <- sd_g1
      summary_stats[[paste0("Median_", group2)]] <- med_g2
      summary_stats[[paste0("Q1_", group2)]] <- q1_g2
      summary_stats[[paste0("Q3_", group2)]] <- q3_g2
      summary_stats[[paste0("Mean_", group2)]] <- mean_g2
      summary_stats[[paste0("SD_", group2)]] <- sd_g2
      
      results[[var]] <- summary_stats
      
    } else if (is.factor(data[[var]]) || is.character(data[[var]])) {
      tab <- table(data[[var]], data[[group_var]])
      
      if (any(tab < 5)) {
        cat_test <- fisher.test(tab)
        test_used <- "Fisher"
      } else {
        cat_test <- chisq.test(tab)
        test_used <- "Chi-squared"
      }
      
      freq_summary <- data.frame(
        Variable = var,
        Median = NA, Q1 = NA, Q3 = NA, Mean = NA, SD = NA,
        Symmetric = NA, Equal_Var = NA,
        Test = test_used,
        P_value = round(cat_test$p.value, 4),
        stringsAsFactors = FALSE
      )
      
      results[[var]] <- freq_summary
    }
  }
  
  final_table <- bind_rows(results)
  return(final_table)
}
