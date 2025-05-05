#' @title stratified_distance_analysis
#' @description
#' \code{\link{stratified_distance_analysis}} This function performs stratified distance matching between patients and controls based on multiple variables. It identifies the closest control group for each patient group using distance-based metrics across defined strata.
#'
#' @param data data.frame that contains numeric or categorical variables arranged in columns.
#' @param brain_cols List of columns that are used in computing distance metrics.
#' @param group_var Column containing categories for the two groups whose distance is being analysed, for example MS patient and healthy control.
#' @param patient_value Value in group_var that identifies patients. Every patient group is matched to the closest control group.
#' @param control_value Value in group_var that identifies controls.
#' @param strat_var Stratification variable. Column indicating subgroups in between witch distance is assessed and matching is done. Factorize to determine plotting order.
#' @param distance_method Metric used to compute difference values. Choose "euclidean", "cosine", or "manhattan".
#' @param profile_method Method to calculate group profiles: Should difference be determined between mean or median of individuals in strata. Choose "mean" or "median".
#' @param plot_title Plot title. Will be auto-generated if NULL.
#' @param include_counts Whether to include subject counts in results table.
#' 
#' @name stratified_distance_analysis
#'
#' @return
#'  \item{warnings}{Non-fatal warnings}
#'  \item{results}{Data frame with distance metrics for all patient-control strata combinations}
#'  \item{top_matches}{Filtered results showing only closest matches for each patient stratum}
#'  \item{heatmap}{Heatmap plot showing distance between all patient and control strata}
#'  \item{gridplot}{Grid plot showing only closest matches}
#'  
#' @examples
#' \dontrun{
#' # Using the function
#' test_results <- stratified_distance_analysis() # By default generates example plots from R test data set mtcars
#' print(test_results$results) # Table of all patient-control strata combinations with distance metrics
#' print(test_results$top_matches) # Only the closest matches for each patient stratum
#' print(test_results$heatmap) # Heatmap visualization of distances between all strata
#' print(test_results$gridplot) # Grid visualization of closest matches only
#'  }                             
#'
#'  @export
stratified_distance_analysis <- function(
    data = mtcars,                # Input data frame 
    brain_cols = c("mpg","cyl","hp"), # Vector of column names to use for distance calculation
    group_var = "am",             # Column that defines patient vs control groups
    patient_value = 1,            # Value in group_var that identifies patients
    control_value = 0,            # Value in group_var that identifies controls
    strat_var = "gear",           # Stratification column to use for similar group matching
    distance_method = "euclidean", # "euclidean", "cosine", or "manhattan"
    profile_method = "mean",      # Method to calculate group profiles: "mean" or "median".
    plot_title = NULL,            # Plot title (if NULL, will be auto-generated)
    include_counts = TRUE         # Whether to include subject counts in results
) {
  
  # Dependencies
  required_packages <- c("dplyr", "ggplot2")
  
  # Check for missing packages
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  if(length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages), "\n")
    install.packages(missing_packages, repos = "https://cran.r-project.org")
  }
  
  require(dplyr)
  require(ggplot2)
  
  # Check if stratification variable is provided
  if (is.null(strat_var)) {
    stop("Stratification variable (strat_var) is required for strata-based matching")
  }
  
  # Validate distance method
  if (!distance_method %in% c("euclidean", "cosine", "manhattan")) {
    stop("Invalid distance method. Choose 'euclidean', 'cosine', or 'manhattan'.")
  }
  
  # Validate profile method
  if (!profile_method %in% c("mean", "median")) {
    stop("Invalid profile method. Choose 'mean' or 'median'.")
  }
  
  # 1. Split data into patients and controls
  patients <- data[data[[group_var]] == patient_value, ]
  controls <- data[data[[group_var]] == control_value, ]
  
  # Remove rows with NA in any brain columns
  patients <- patients[complete.cases(patients[, brain_cols]), ]
  controls <- controls[complete.cases(controls[, brain_cols]), ]
  
  if (nrow(patients) == 0 && nrow(controls) == 0) {
    stop("No valid data after removing NA values in brain columns")
  }
  
  # 2. Get ALL possible stratification groups to handle empty strata
  if (is.factor(data[[strat_var]])) {
    all_strata <- levels(data[[strat_var]])
  } else {
    all_strata <- sort(unique(data[[strat_var]]))
  }
  
  # Get actual strata present in the filtered data
  patient_strata <- unique(patients[[strat_var]])
  control_strata <- unique(controls[[strat_var]])
  
  # Create cross-product of all possible strata combinations
  strat_combos <- expand.grid(
    patient_stratum = all_strata,
    control_stratum = all_strata,
    stringsAsFactors = FALSE
  )
  
  # 3. Normalize data before calculating profiles
  normalize_data <- function(df) {
    df_normalized <- df
    for (col in brain_cols) {
      df_normalized[[col]] <- scale(df[[col]])
    }
    return(df_normalized)
  }
  
  # Only normalize if there's data to normalize
  if (nrow(patients) > 0) {
    patients_norm <- normalize_data(patients)
  } else {
    patients_norm <- patients  # Empty data frame
  }
  
  if (nrow(controls) > 0) {
    controls_norm <- normalize_data(controls)
  } else {
    controls_norm <- controls  # Empty data frame
  }
  
  # 4. Calculate profiles for each group (including empty strata)
  patient_profiles <- list()
  control_profiles <- list()
  
  # Create a default profile of all zeros for empty strata
  default_profile <- rep(0, length(brain_cols))
  names(default_profile) <- brain_cols
  
  # Calculate patient group profiles for ALL possible strata
  for (stratum in all_strata) {
    # Check if this stratum exists in patient data
    if (stratum %in% patient_strata) {
      subset_data <- patients_norm[patients_norm[[strat_var]] == stratum, ]
      
      # Calculate profile (mean or median) for each brain column
      if (profile_method == "mean") {
        profile_values <- sapply(brain_cols, function(col) mean(subset_data[[col]], na.rm = TRUE))
      } else { # profile_method == "median"
        profile_values <- sapply(brain_cols, function(col) median(subset_data[[col]], na.rm = TRUE))
      }
      
      # Count subjects in this group
      count <- nrow(subset_data)
    } else {
      # For empty strata, use default profile
      profile_values <- default_profile
      count <- 0
    }
    
    patient_profiles[[as.character(stratum)]] <- list(
      profile = profile_values,
      count = count
    )
  }
  
  # Calculate control group profiles for ALL possible strata
  for (stratum in all_strata) {
    # Check if this stratum exists in control data
    if (stratum %in% control_strata) {
      subset_data <- controls_norm[controls_norm[[strat_var]] == stratum, ]
      
      # Calculate profile (mean or median) for each brain column
      if (profile_method == "mean") {
        profile_values <- sapply(brain_cols, function(col) mean(subset_data[[col]], na.rm = TRUE))
      } else { # profile_method == "median"
        profile_values <- sapply(brain_cols, function(col) median(subset_data[[col]], na.rm = TRUE))
      }
      
      # Count subjects in this group
      count <- nrow(subset_data)
    } else {
      # For empty strata, use default profile
      profile_values <- default_profile
      count <- 0
    }
    
    control_profiles[[as.character(stratum)]] <- list(
      profile = profile_values,
      count = count
    )
  }
  
  # 5. Create function to calculate distance using specified metric
  calculate_distance <- function(v1, v2, method) {
    # Handle cases where profiles contain NaN (from empty strata)
    v1[is.nan(v1)] <- 0
    v2[is.nan(v2)] <- 0
    
    if (method == "euclidean") {
      return(sqrt(sum((v1 - v2)^2)))
    } else if (method == "cosine") {
      # Avoid division by zero
      if (all(v1 == 0) || all(v2 == 0)) {
        return(1) # Maximum distance
      }
      
      cosine_similarity <- sum(v1 * v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))
      return(1 - cosine_similarity) # Convert to distance
    } else if (method == "manhattan") {
      return(sum(abs(v1 - v2)))
    } else {
      stop("Invalid distance method. Choose 'euclidean', 'cosine', or 'manhattan'.")
    }
  }
  
  # 6. Create results dataframe
  results_cols <- c("Patient_Stratum", "Control_Stratum", "Distance")
  
  if (include_counts) {
    results_cols <- c(results_cols, "Patient_Count", "Control_Count")
  }
  
  results <- data.frame(matrix(ncol = length(results_cols), nrow = nrow(strat_combos)))
  colnames(results) <- results_cols
  
  # Fill results with comparison data for all combinations
  for (i in 1:nrow(strat_combos)) {
    p_stratum <- strat_combos$patient_stratum[i]
    c_stratum <- strat_combos$control_stratum[i]
    
    p_key <- as.character(p_stratum)
    c_key <- as.character(c_stratum)
    
    # Check if either stratum is empty
    p_count <- patient_profiles[[p_key]]$count
    c_count <- control_profiles[[c_key]]$count
    
    if (p_count == 0 || c_count == 0) {
      # Set to NA for empty strata comparisons
      dist_value <- NA
    } else {
      # Calculate distance between profiles only for non-empty strata
      dist_value <- calculate_distance(
        patient_profiles[[p_key]]$profile, 
        control_profiles[[c_key]]$profile, 
        distance_method
      )
    }
    
    # Fill row in results
    results$Patient_Stratum[i] <- paste(p_stratum)
    results$Control_Stratum[i] <- paste(c_stratum)
    results$Distance[i] <- dist_value
    
    if (include_counts) {
      results$Patient_Count[i] <- p_count
      results$Control_Count[i] <- c_count
    }
  }
  
  # Ensure strata are ordered correctly in the results
  if (is.factor(data[[strat_var]])) {
    results$Patient_Stratum <- factor(results$Patient_Stratum, levels = levels(data[[strat_var]]))
    results$Control_Stratum <- factor(results$Control_Stratum, levels = levels(data[[strat_var]]))
    warning_messages <- NA
  } else {
    results$Patient_Stratum <- factor(results$Patient_Stratum, levels = all_strata)
    results$Control_Stratum <- factor(results$Control_Stratum, levels = all_strata)
    warning_messages <- "Warning: strat_var has no levels and will be plotted alpahbetically. Factorize strat_var to specify plotting order: data$strat_var <- factor(data$strat_var, levels = c('strata1', 'strata2', 'strata3'))"
  }
  
  # 7. Create filtered results with only lowest distance for each patient stratum
  top_matches <- data.frame()
  
  for (p_stratum in levels(results$Patient_Stratum)) {
    # Get all rows for this patient stratum
    p_rows <- results[results$Patient_Stratum == p_stratum, ]
    
    # Check if this patient stratum is empty
    p_count <- p_rows$Patient_Count[1]
    
    if (p_count == 0) {
      # For empty patient strata, add a row with NA distance
      # Find any control stratum that's not empty if possible
      non_empty_control <- p_rows[!is.na(p_rows$Distance), ]
      
      if (nrow(non_empty_control) > 0) {
        # Use the first non-empty control
        min_row <- non_empty_control[1, ]
      } else {
        # All controls empty, just use the first row
        min_row <- p_rows[1, ]
      }
    } else {
      # For non-empty patient strata, find control with min distance
      # Exclude NA values (empty control strata)
      valid_rows <- p_rows[!is.na(p_rows$Distance), ]
      
      if (nrow(valid_rows) > 0) {
        # Find the row with minimum distance
        min_row <- valid_rows[which.min(valid_rows$Distance), ]
      } else {
        # All controls are empty, use the first row
        min_row <- p_rows[1, ]
      }
    }
    
    # Add to top matches
    top_matches <- rbind(top_matches, min_row)
  }
  
  # 8. Create visualization
  # Generate plot title if not provided
  if (is.null(plot_title)) {
    cols_str <- paste(brain_cols, collapse = ", ")
    if (nchar(cols_str) > 40) {
      cols_str <- paste0(substr(cols_str, 1, 37), "...")
    }
    
    strat_name <- gsub("_", " ", strat_var)
    plot_title <- paste0("Stratified Distance (by ", strat_name, ", ", profile_method, " profiles)\n",
                         "Using: ", cols_str)
  }
  
  # Create labels with counts for each stratum
  # Get unique patient counts for each stratum
  patient_count_map <- list()
  for (i in 1:nrow(results)) {
    stratum <- as.character(results$Patient_Stratum[i])
    count <- results$Patient_Count[i]
    patient_count_map[[stratum]] <- count
  }
  
  # Get unique control counts for each stratum
  control_count_map <- list()
  for (i in 1:nrow(results)) {
    stratum <- as.character(results$Control_Stratum[i])
    count <- results$Control_Count[i]
    control_count_map[[stratum]] <- count
  }
  
  # Create vectors of labels with counts
  patient_labels <- sapply(levels(results$Patient_Stratum), 
                           function(s) paste0(s, "\n(n=", patient_count_map[[as.character(s)]], ")"))
  
  control_labels <- sapply(levels(results$Control_Stratum), 
                           function(s) paste0(s, "\n(n=", control_count_map[[as.character(s)]], ")"))
  
  # Create heatmap visualization with count labels - lower distance is better (lighter colors)
  heatmap_plot <- ggplot2::ggplot(results, ggplot2::aes(x = Patient_Stratum, 
                                                        y = Control_Stratum,
                                                        fill = Distance)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(Distance), 
                                                   "-", 
                                                   round(Distance, 2))), 
                       color = "black", size = 3) +
    # White/light blue for low distance (better matches), dark blue for high distance
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue", na.value = "grey90") +
    ggplot2::labs(title = plot_title,
                  x = paste("Patient", gsub("_", " ", strat_var)),
                  y = paste("Control", gsub("_", " ", strat_var)),
                  fill = paste0("Distance \n(", distance_method, ")\nSmaller = Better Match")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    # Add custom axis labels with counts
    ggplot2::scale_x_discrete(labels = patient_labels) +
    ggplot2::scale_y_discrete(labels = control_labels) +
    # Reverse the size scale so smaller points represent smaller distances
    ggplot2::scale_size(range = c(10, 1))
  
  # Create a grid with all strata combinations
  all_strata_factor <- levels(factor(results$Patient_Stratum))
  
  # Create a complete plot data frame starting with all possible combinations
  plot_data <- expand.grid(
    Patient_Stratum = all_strata_factor,
    Control_Stratum = all_strata_factor,
    stringsAsFactors = TRUE
  )
  
  # Add Distance column initialized to NA
  plot_data$Distance <- NA
  
  # Now update only the cells that have top matches
  for (i in 1:nrow(top_matches)) {
    p_strat <- as.character(top_matches$Patient_Stratum[i])
    c_strat <- as.character(top_matches$Control_Stratum[i])
    dist_val <- top_matches$Distance[i]
    
    # Find the matching row in plot_data and update it
    match_idx <- which(plot_data$Patient_Stratum == p_strat & 
                         plot_data$Control_Stratum == c_strat)
    
    if (length(match_idx) > 0) {
      plot_data$Distance[match_idx] <- dist_val
    }
  }
  
  # Add other columns from top_matches if needed for the plot
  if (include_counts) {
    plot_data$Patient_Count <- NA
    plot_data$Control_Count <- NA
    
    for (i in 1:nrow(top_matches)) {
      p_strat <- as.character(top_matches$Patient_Stratum[i])
      c_strat <- as.character(top_matches$Control_Stratum[i])
      
      match_idx <- which(plot_data$Patient_Stratum == p_strat & 
                           plot_data$Control_Stratum == c_strat)
      
      if (length(match_idx) > 0) {
        plot_data$Patient_Count[match_idx] <- top_matches$Patient_Count[i]
        plot_data$Control_Count[match_idx] <- top_matches$Control_Count[i]
      }
    }
  }
  
  # Create the best matches grid plot with count labels
  # Add a column to indicate empty strata combinations
  plot_data$IsEmpty <- FALSE
  
  # Update top_matches for empty strata
  for (i in 1:nrow(top_matches)) {
    p_strat <- as.character(top_matches$Patient_Stratum[i])
    c_strat <- as.character(top_matches$Control_Stratum[i])
    
    match_idx <- which(plot_data$Patient_Stratum == p_strat & 
                         plot_data$Control_Stratum == c_strat)
    
    # If this is a comparison with empty strata
    if (is.na(top_matches$Distance[i])) {
      plot_data$IsEmpty[match_idx] <- TRUE
    }
  }
  
  # Use distance directly for plotting
  # Smaller points = smaller distance = better match
  
  # Create the plot with special handling for empty strata
  best_matches_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Patient_Stratum, 
                                                              y = Control_Stratum,
                                                              size = Distance)) +
    # For non-empty strata
    ggplot2::geom_point(data = subset(plot_data, !IsEmpty), 
                        alpha = 0.7, na.rm = TRUE) +
    # For empty strata, add a different marker
    ggplot2::geom_point(data = subset(plot_data, IsEmpty),
                        shape = 4, size = 3) +
    # Regular text labels for non-empty strata
    ggplot2::geom_text(data = subset(plot_data, !IsEmpty),
                       ggplot2::aes(label = ifelse(!is.na(Distance), 
                                                  round(Distance, 2), "")), 
                       vjust = 2, size = 3) +
    # "Empty" label for empty strata
    ggplot2::geom_text(data = subset(plot_data, IsEmpty),
                       label = "-", 
                       vjust = 2, size = 3) +
    ggplot2::labs(title = paste0(plot_title),
                  x = paste("Patient", gsub("_", " ", strat_var)),
                  y = paste("Control", gsub("_", " ", strat_var)),
                  size = paste0("Distance\nSmaller = Better Match\n(", distance_method, ")")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    # Add custom axis labels with counts
    ggplot2::scale_x_discrete(labels = patient_labels) +
    ggplot2::scale_y_discrete(labels = control_labels)
  
  # 10. Return results and plots in a structured list
  if (!exists("warning_messages")) {warning_messages <- NA}
  return(list(
    warnings = warning_messages,   # Non-fatal warnings
    results = results,             # All strata combinations
    top_matches = top_matches,     # Filtered results with best matches only
    heatmap = heatmap_plot,        # Heatmap showing all comparisons
    gridplot = best_matches_plot   # Grid plot showing only best matches
  ))
}