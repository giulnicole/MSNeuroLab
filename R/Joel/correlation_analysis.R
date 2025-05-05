#' @title correlation_analysis
#' @description
#' \code{\link{correlation_analysis}} This convenient function performs correlation analysis between sets of variables,
#' calculating correlation coefficients and confidence intervals grouped by a specified variable.
#'
#' @param input_data data.frame containing the variables to analyze. Defaults to mtcars.
#' @param dependent Vector of one or several column names to use as dependent variables.
#' @param independent Vector of one or several column names to use as independent variables.
#' @param method Correlation method to use. Choose "pearson", "spearman", or "kendall". Defaults to "spearman".
#' @param group_column Column name that identifies different groups in the data. Defaults to "am" in mtcars example.
#'
#' @name correlation_analysis
#'
#' @return
#'  \item{result}{A data frame of correlation results with coefficients, confidence intervals, and p-values for each group}
#'
#' @examples
#' \dontrun{
#' # Using the function with default parameters
#' corr_results <- correlation_analysis() # By default uses mtcars data with mpg as dependent and wt, hp as independent
#' print(corr_results) # Table of correlation coefficients, confidence intervals, and p-values
#' }
#'
#' @export
correlation_analysis <- function(
    input_data = mtcars,                          # Input data frame
    dependent = "mpg",                            # Dependent variable(s)
    independent = c("wt", "hp"),                  # Independent variables
    method = "spearman",                          # Correlation method: "pearson", "spearman", or "kendall"
    group_column = "am"                           # Column that defines groups
) {
  
  # Dependencies
  required_packages <- c("dplyr","psych","rlang")
  
  # Check for missing packages
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  if(length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages), "\n")
    install.packages(missing_packages, repos = "https://cran.r-project.org")
  }
  
  # Load required packages
  require(dplyr)
  require(psych)
  
  # Input validation
  if (!is.data.frame(input_data)) {
    stop("input_data must be a data frame")
  }
  
  if (!all(dependent %in% colnames(input_data))) {
    stop("Not all dependent variables exist in the input data")
  }
  
  if (!all(independent %in% colnames(input_data))) {
    stop("Not all independent variables exist in the input data")
  }
  
  if (!method %in% c("pearson", "spearman", "kendall")) {
    stop("Invalid correlation method. Choose 'pearson', 'spearman', or 'kendall'.")
  }
  
  if (!group_column %in% colnames(input_data)) {
    stop("Group column does not exist in the input data")
  }
  
  # Create dataframe for saving results
  correlation_results <- data.frame(
    Variable1 = character(),
    Variable2 = character(),
    CorrelationCoefficient = numeric(),
    RawLower = numeric(),
    RawUpper = numeric(),
    P_value = numeric(),
    AdjLower = numeric(),
    AdjUpper = numeric(),
    Group = character(),
    stringsAsFactors = FALSE
  )
  
  # Check if only numeric variables are being used for correlation
  for (var in c(dependent, independent)) {
    if (!is.numeric(input_data[[var]])) {
      stop(paste("Variable", var, "is not numeric. Correlation requires numeric variables."))
    }
  }
  
  # Process each group
  for (group_loop in unique(input_data[[group_column]])) {
    # Filter data for this group
    collection <- input_data %>% dplyr::filter(!!rlang::sym(group_column) == group_loop)
    
    # Skip groups with insufficient data
    if (nrow(collection) < 3) {
      warning(paste("Group", group_loop, "has fewer than 3 observations. Skipping."))
      next
    }
    
    # Calculate correlations
    tryCatch({
      correlation_score <- psych::corr.test(
        collection[independent],
        collection[dependent],
        method = method
      )
      
      # Store results for each variable pair
      for (i in seq_along(independent)) {
        for (j in seq_along(dependent)) {
          correlation_value <- correlation_score$r[i, j] # $r is table with independent as row, dependent as column 
          
          raw_lower <- correlation_score$ci$lower[i+(i*(j-1))] # $lower is a column that contains lower ci limits for all correlations as rows. Inconvenient yes
          raw_upper <- correlation_score$ci$upper[i+(i*(j-1))] # $upper is a column that contains upper ci limits for all correlations as rows.
          
          p_value <- correlation_score$p[i, j] # $p is table with independent as row, dependent as column 
          
          adj_lower <- correlation_score$ci.adj$lower[i+(i*(j-1))] # $lower is a column that contains lower ci limits for all correlations as rows.
          adj_upper <- correlation_score$ci.adj$upper[i+(i*(j-1))] # $upper is a column that contains upper ci limits for all correlations as rows.
          
          # Add row to results
          correlation_results <- rbind(correlation_results, data.frame(
            Variable1 = independent[i],
            Variable2 = dependent[j],
            CorrelationCoefficient = correlation_value,
            RawLower = raw_lower,
            RawUpper = raw_upper,
            P_value = p_value,
            AdjLower = adj_lower,
            AdjUpper = adj_upper,
            Group = as.character(group_loop),
            stringsAsFactors = FALSE
          ))
        }
      }
    }, error = function(e) {
      warning(paste("Error calculating correlation for group", group_loop, ":", e$message))
    })
  }
  
  # Add significance indicators
  correlation_results$Significance <- NA
  correlation_results$Significance[correlation_results$P_value >= 0.05] <- "ns"
  correlation_results$Significance[correlation_results$P_value < 0.05] <- "*"
  correlation_results$Significance[correlation_results$P_value < 0.01] <- "**"
  correlation_results$Significance[correlation_results$P_value < 0.001] <- "***"
  
  return(correlation_results)
}