#' @title test_with_levene
#' @description
#' \code{\link{test_with_levene}} Function for comparing statistical comparison between numeric variables, testing for assumptions on varaibles' distribution and categorical variables
#'
#' @param data data.frame object with the variables (numeric or categorical) on which performing the statistical comparisons
#' @param group_var grouping variable with two groups (classes) in quotes
#' 
#' @name test_with_levene
#'
#' @return
#'  \item{final_table}{a data.frame object with results unlisted (with bindrow function)}
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
#'  }                             
#'
#'
#'  
#'  
#'  @export
test_with_levene <- function(data, group_var) {
  
  suppressWarnings({
  require(car)
  require(tidyverse)
  require(dplyr)
  require(kableExtra)  })
  
  if (!group_var %in% names(data)) stop("group_var must be a column in the data")
  
  results <- list()
  
  for (var in names(data)) {
    if (var == group_var) next
    
    if (is.numeric(data[[var]])) {
      summary_stats <- data.frame(
        Variable = var,
        Median = round(median(data[[var]], na.rm = TRUE),2),
        IQR = round(IQR(data[[var]], na.rm = TRUE),2),
        Mean = round(mean(data[[var]], na.rm = TRUE),2),
        SD = round(sd(data[[var]], na.rm = TRUE),2),
        stringsAsFactors = FALSE
      )
      
      groups <- na.omit(unique(data[[group_var]]))
      if (length(groups) != 2) next # Only works for 2 groups
      
      normality <- sapply(groups, function(g) {
        vals <- data[[var]][data[[group_var]] == g]
        if (length(vals) >= 3) {
          shapiro.test(vals)$p.value
        } else {
          NA
        }
      })
      
      symmetric <- all(normality > 0.05, na.rm = TRUE)
      
      x <- data[[var]][data[[group_var]] == groups[1]]
      y <- data[[var]][data[[group_var]] == groups[2]]
      
      # Levene's test for homogeneity of variance
      lev_test <- leveneTest(data[[var]] ~ as.factor(data[[group_var]]), data = data)
      equal_var <- lev_test$`Pr(>F)`[1] > 0.05
      
      if (symmetric) {
        if (equal_var) {
          test_res <- t.test(x, y, var.equal = TRUE) # classical t-test
          test_used <- "t-test (equal var)"
        } else {
          test_res <- t.test(x, y, var.equal = FALSE) # Welch's t-test
          test_used <- "Welch's t-test (unequal var)"
        }
      } else {
        test_res <- wilcox.test(x, y)
        test_used <- "Wilcoxon"
      }
      
      summary_stats$Symmetric <- ifelse(symmetric, "Yes", "No")
      summary_stats$Equal_Var <- ifelse(equal_var, "Yes", "No")
      summary_stats$Test <- test_used
      summary_stats$P_value <- round(test_res$p.value, 4)
      
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
        Median = NA, IQR = NA, Mean = NA, SD = NA,
        Symmetric = NA, Equal_Var = NA,
        Test = test_used,
        P_value = round(cat_test$p.value, 4),
        stringsAsFactors = FALSE
      )
      
      results[[var]] <- freq_summary
    }
  }
  
  # Bind all rows together
  final_table <- bind_rows(results)
  return(final_table)
}




