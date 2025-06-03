#' @title run_univariate_logistic
#' @description
#' \code{\link{run_univariate_logistic}} This function performs univariate logistic regression analyses for a set of predictor variables against a binary outcome variable in a given dataset. It returns a tidy data frame with odds ratios (OR), 95% confidence intervals (CI), and p-values for each predictor.
#'
#' @param data A data frame containing the outcome and predictor variables.
#' @param outcome_var A string specifying the name of the binary outcome variable.
#'                    If the variable is a factor or character, values equal to "Yes" are
#'                    recoded to 1 and others to 0.
#' @param predictors A character vector specifying the names of the predictor variables to test.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{Predictor}{Name of the predictor variable}
#'   \item{OR}{Estimated odds ratio}
#'   \item{CI_low}{Lower bound of the 95% confidence interval for the OR}
#'   \item{CI_high}{Upper bound of the 95% confidence interval for the OR}
#'   \item{p_value}{p-value for the predictor in the logistic model}
#' }
#' 
#' @examples
#' 
#' library(readxl)
#' # Reading the data
#' data <- read_excel("Data/examination_dataset1.xlsx")
#' # Selecting variables to test at univariate 
#' variables_to_test <- colnames(data)[c(3:8)]
#' # Using the function
#' res_univariates <- run_univariate_logistic(data, "smoker", predictors = variables_to_test)
#'
#'
#'
#'
#'@export
run_univariate_logistic <- function(data, outcome_var, predictors) {
  # Ensure outcome variable is binary numeric (0/1)
  if (is.character(data[[outcome_var]]) || is.factor(data[[outcome_var]])) {
    data[[outcome_var]] <- as.numeric(data[[outcome_var]] == "Yes")
  }
  
  # Initialize empty results table
  results <- data.frame(
    Predictor = character(),
    OR = numeric(),
    CI_low = numeric(),
    CI_high = numeric(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (var in predictors) {
    formula <- as.formula(paste(outcome_var, "~", var))
    model <- glm(formula, data = data, family = binomial)
    
    coef_summary <- summary(model)$coefficients
    OR <- exp(coef(model)[2])  # Odds ratio
    CI <- exp(confint(model)[2, ])  # 95% CI
    p_val <- coef_summary[2, 4]  # p-value
    
    # Add row to results table
    results <- rbind(results, data.frame(
      Predictor = var,
      OR = round(OR, 3),
      CI_low = round(CI[1], 3),
      CI_high = round(CI[2], 3),
      p_value = round(p_val, 4)
    ))
  }
  
  return(results)
}



