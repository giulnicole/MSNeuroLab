#' @title run_plsda_clean
#' @description
#' \code{\link{run_plsda_clean}} Function for cleaning the dataset and running partial least squares discriminant analysis
#'
#' This function separates categorical and numeric variables from a given dataframe
#' scales them, and runs PLS-DA using the mixOmics package.
#' It also plots the individual samples and  variables based on a cutoff of correlation.
#'
#' @param data A dataframe containing numeric and possibly non-numeric variables.
#' @param group A factor or a vector that can be coerced to a factor, representing class labels.
#' @param ncomp Number of PLS components to compute. Default is 4.
#' @param var.cutoff Cutoff for variable correlation between covariates in plotVar. Default is 0.6.
#' @return A list containing the plsda model and the generated plots.
#' @import mixOmics
#' 
#' @examples
#' \dontrun{
#' 
#' library(mixOmics)
#' library(readxl)
#' 
#' # Reading the data
#' data<- read_excel("Data/examination_dataset1.xlsx")
#'  
#'  # Using the function for separating variables
#'  data.plsda <- data_for_plsda(data)
#'  
#' # Using the function for PLSDA
#' plsda <- run_plsda_clean(data = data, group=data$sex, ncomp = 2, var.cutoff = 0.6)
#'
#'  }  
#' 
#' 
#' @export
run_plsda_clean <- function(data, group, ncomp = 3, var.cutoff = 0.6) {
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
  
  
  return(plsda_model)
}



data_for_plsda <- function(data){
  
  suppressWarnings({
    require(mixOmics)
    require(tidyverse)
    require(dplyr)
    require(ggplot2)
  })
  
  
  # Convert character columns to factors
  data.num <- data %>% dplyr::select(where(is.numeric))
  
  # Convert character columns to factors
  data.cat <- data %>%
    mutate(across(where(is.character), as.factor))
  
  
  return(list(Numeric = data.num, Factors = data.cat))
}




