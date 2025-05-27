#' Run PLS-DA on Scaled Numeric Variables
#'
#' This function selects numeric variables from a given dataframe,
#' scales them, and runs PLS-DA using the mixOmics package.
#' It also plots the individual samples and important variables based on a cutoff.
#'
#' @param data A dataframe containing numeric and possibly non-numeric variables.
#' @param group A factor or a vector that can be coerced to a factor, representing class labels.
#' @param ncomp Number of PLS components to compute. Default is 4.
#' @param var.cutoff Cutoff for variable importance in plotVar. Default is 0.6.
#' @return A list containing the plsda model and the generated plots.
#' @import mixOmics
#' @export
run_plsda_clean <- function(data, group, ncomp = 4, var.cutoff = 0.6) {
  # Load required package
  if (!requireNamespace("mixOmics", quietly = TRUE)) {
    stop("The 'mixOmics' package is required but not installed.")
  }
  
  # Select numeric columns only
  numeric_data <- data[sapply(data, is.numeric)]
  
  # Scale the numeric data
  scaled_data <- scale(numeric_data)
  
  # Ensure group is a factor
  group <- as.factor(group)
  
  # Run PLS-DA
  plsda_model <- mixOmics::plsda(scaled_data, group, ncomp = ncomp)
  
  # Plot individuals
  plotIndiv(plsda_model, group = group)
  
  # Plot variables with a cutoff
  plotVar(plsda_model, cutoff = var.cutoff)
  
  # Return model and plots invisibly
  invisible(list(model = plsda_model))
}
