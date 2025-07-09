#' @title run_univariate_logistic_repeated
#' @description
#' \code{\link{run_univariate_logistic_repeated}} This function performs univariate logistic regression analyses for a set of predictor variables against a binary outcome variable for repeated measures in a given dataset. It returns a tidy data frame with odds ratios (OR), 95% confidence intervals (CI), and p-values for each predictor.
#' 
#' @param data A data frame containing the outcome and predictor variables.
#' @param outcome_var A string specifying the name of the binary outcome variable.
#'                    If the variable is a factor or character, values equal to "Yes" are
#'                    recoded to 1 and others to 0.
#' @param predictors A character vector specifying the names of the predictor variables to test.
#' @param time is the timepoint for the repeated measures
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
#' # Reading the data
#' data<- MSNeuroLab::data
#' # Selecting variables to test at univariate 
#' variables_to_test <- colnames(data)[c(3:8)]
#' # Using the function
#' res_univariates <- run_univariate_logistic(data, "smoker", predictors = variables_to_test, Time)
#'
#'
#'
#'
#'@export
run_univariate_logistic_repeated <- function(data, outcome_var, predictors, time) {
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
    formula <- as.formula(paste(outcome_var, "~",  time,  "+ (1 | ID) + " , var))
    model <- glmer(formula, data, family = binomial)
    
    print("ok")
    coef_summary <- as.data.frame(summary(model)$coefficients)
    OR <- exp(coef_summary[5,1])  # Odds ratio
    SE <- exp(coef_summary[5,2])
    CI_low <- exp(log(OR) -1.96*SE)  # 95% CI
    CI_high <- exp(log(OR) +1.96*SE)
    p_val <- coef_summary[5,4]  # p-value
    
    # Add row to results table
    results <- rbind(results, data.frame(
      Predictor = var,
      OR = round(OR, 3),
      CI_low = round(CI_low, 3),
      CI_high = round(CI_high, 3),
      p_value = round(p_val, 4)
    ))
  }
  
  return(results)
}


