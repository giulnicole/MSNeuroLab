#' @title similarity_analysis
#' @description
#' \code{\link{similarity_analysis}} This function performs similarity matching between patients and controls based on multiple variables. It identifies the most similar control subject for each patient using distance-based similarity metrics. 
#'
#' @param data data.frame that contains numeric or categorical variables arranged in columns.
#' @param input_columns List of columns that are used in computing distance metrics.
#' @param group_column Column containing categories for the two groups whose similarity in being analysed, for example MS patient and healthy control.
#' @param patient_value Value in group_column that identifies patients. Every patient is matched to one control. Several patients may match the same control.
#' @param control_value Value in group_column that identifies controls. Every control may not be match to any patient.
#' @param comparison_variable Compare this variable between patients and their most similar controls.
#' @param distance_metric # Metric used to compute difference values. Choose "euclidean", "cosine", or "manhattan".
#' @param x_label Variable to plot on x-axis. Variable name is used if NULL.
#' @param y_label Variable to plot on y-axis. Variable name is used if NULL.
#' @param plot_title Plot title. Will be auto-generated if NULL.
#' @param include_id Whether to include patient/control IDs in results table.
#' 
#' @name similarity_analysis
#'
#' @return
#'  \item{result}{List of a table with numeric comparison_variable values, distance metrics and similarities for pairs of most similar patients and controls, and a scatterplot visualizing the comparisons}
#'  
#' @examples
#' \dontrun{
#' # Using the function
#' test_results <- similarity_analysis() # By default generates an example plot from R test data set mtcars
#' print(test_results$results) # Table of comparison_variable values, distance metrics and similarities for pairs of most similar patients and controls 
#' print(test_results$plot) # Scatterplot showing correlation in comparison_variable between the most similar patients and controls with one dot for each patient-control comparison. 
#' # One dot will match each patient. Each control is matched by none to several dots. Dot size indicates level of similarity calculated from input columns. 
#' # The farther away from the dotted line the dot lands, the bigger the discrepancy in comparison_variable between the patient and the control.
#'  }                             
#'
#'  @export
similarity_analysis <- function(
    data = mtcars,             # Input data frame 
    input_columns = c("mpg","cyl","hp"), # Vector of columns to use for determining similarity
    group_column = "am",  # Column that defines patient vs control groups
    patient_value = 1,      # Value in group_column that identifies patients
    control_value = 0,      # Value in group_column that identifies controls
    comparison_variable = "disp",           # Variable to compare (can be any numeric column)
    distance_metric = "euclidean", # "euclidean", "cosine", or "manhattan"
    x_label = NULL,             # Variable to plot on x-axis (column in results), NULL for automatic
    y_label = NULL,             # Variable to plot on y-axis (column in results), NULL for automatic
    plot_title = NULL,        # Plot title (if NULL, will be auto-generated)
    include_id = FALSE        # Whether to include patient/control IDs in results table
) {
  
  # Dependencies
  required_packages <- c("dplyr","ggplot2")
  
  # Check for missing packages
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  if(length(missing_packages) > 0) {
    cat("Installing missing packages:",paste(missing_packages),"\n")
    install.packages(missing_packages, repos = "https://cran.r-project.org")} # Install missing packages
  
  require(dplyr)
  require(ggplot2)
  
  # Automatically set x_label and y_label based on comparison_variable if not specified
  if (is.null(x_label)) {
    x_label <- paste0("Patient_", comparison_variable)
  }
  
  if (is.null(y_label)) {
    y_label <- paste0("Matching_Control_", comparison_variable)
  }
  
  # Determine column names for results
  result_cols <- c(x_label, y_label, "Distance")
  if (include_id && "ID" %in% names(data)) {
    result_cols <- c(result_cols, "Patient_ID", "Control_ID")
  }
  
  # Create empty results dataframe with the correct column names
  results <- data.frame(matrix(ncol = length(result_cols), nrow = 0))
  colnames(results) <- result_cols
  
  # 1. Split data into patients and controls
  patients <- data[data[[group_column]] == patient_value, ]
  controls <- data[data[[group_column]] == control_value, ]
  
  # Remove rows with NA in any input columns
  patients <- patients[complete.cases(patients[, input_columns]), ]
  controls <- controls[complete.cases(controls[, input_columns]), ]
  
  if (nrow(patients) == 0 || nrow(controls) == 0) {
    stop("No valid data after removing NA values in brain columns")
  }
  
  # 2. Normalize attributes to make them comparable
  normalize_data <- function(df) {
    df_normalized <- df
    for (col in input_columns) {
      df_normalized[[col]] <- scale(df[[col]])
    }
    return(df_normalized)
  }
  
  patients_norm <- normalize_data(patients)
  controls_norm <- normalize_data(controls)
  
  # 3. Create function to calculate distance using specified metric
  calculate_distance <- function(v1, v2, method) {
    if (method == "euclidean") {
      return(sqrt(sum((v1 - v2)^2)))
    } else if (method == "cosine") {
      similarity <- sum(v1 * v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))
      return(1 - similarity) # Convert to distance
    } else if (method == "manhattan") {
      return(sum(abs(v1 - v2)))
    } else {
      stop("Invalid distance method. Choose 'euclidean', 'cosine', or 'manhattan'.")
    }
  }
  
  # 4. For EACH INDIVIDUAL patient, find most similar control
  for (i in 1:nrow(patients_norm)) {
    # Get this individual patient's profile
    p_profile <- as.numeric(patients_norm[i, input_columns])
    p_value <- patients_norm[i, comparison_variable]
    p_id <- if ("ID" %in% names(patients_norm)) patients_norm$ID[i] else i
    
    # Calculate distance to EACH INDIVIDUAL control
    control_distances <- data.frame(
      Control_Value = controls_norm[[comparison_variable]],
      Distance = NA_real_,
      Control_ID = if ("ID" %in% names(controls_norm)) controls_norm$ID else 1:nrow(controls_norm)
    )
    
    for (j in 1:nrow(controls_norm)) {
      # Get this individual control's profile
      c_profile <- as.numeric(controls_norm[j, input_columns])
      
      # Calculate distance between this patient and this control
      control_distances$Distance[j] <- calculate_distance(p_profile, c_profile, distance_metric)
    }
    
    # Find closest match for this patient
    best_match <- control_distances[order(control_distances$Distance), , drop = FALSE][1, ]
    
    # Create new row for results
    new_row <- data.frame(matrix(ncol = length(result_cols), nrow = 1))
    colnames(new_row) <- result_cols
    
    new_row[1, x_label] <- p_value
    new_row[1, y_label] <- best_match$Control_Value
    new_row[1, "Distance"] <- best_match$Distance
    
    if (include_id && "ID" %in% names(data)) {
      new_row[1, "Patient_ID"] <- p_id
      new_row[1, "Control_ID"] <- best_match$Control_ID
    }
    
    # Append to results
    results <- rbind(results, new_row)
  }
  
  # Add Similarity column for plotting
  results$Similarity <- 1/results$Distance
  
  # 5. Create and return the visualization
  # Generate plot title if not provided
  if (is.null(plot_title)) {
    cols_str <- paste(input_columns, collapse = ", ")
    if (nchar(cols_str) > 40) {
      cols_str <- paste0(substr(cols_str, 1, 37), "...")
    }
    var_name <- gsub("_", " ", comparison_variable)
    plot_title <- paste0("Individual Brain Similarity by ", var_name, "\n",
                         "Using: ", cols_str)
  }
  
  # Create plot
  p <- ggplot2::ggplot(results, ggplot2::aes_string(x = x_label, y = y_label, size = "Similarity")) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::labs(title = plot_title,
                  x = gsub("_", " ", x_label),  # Make axis labels more readable
                  y = gsub("_", " ", y_label),
                  size = paste0("Similarity \n(", distance_metric, ")")) +
    ggplot2::theme_minimal()
  
  # Return both results and plot
  result <- list(results = results, plot = p)
  return(result)
}