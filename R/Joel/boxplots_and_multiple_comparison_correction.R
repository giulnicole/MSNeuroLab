boxplots_and_multiple_correction <- function(input_data,
                                             columns_to_analyze = NULL, # The columns to include in analysis
                                             comparison_list = NULL, # (Optional) Limit group wise comparisons to the ones in this list, 
                                                                     # otherwise do all possible comparisons
                                             y_unit = "", # Add text to the end of axis labels
                                             x_unit = "", # Add text to the end of axis labels
                                             p_value_type = "wilcox_p_benjaminihochberg_rounded", # Options:"wilcox_p",
                                                                                                  #         "wilcox_p_benjaminihochberg_rounded",
                                                                                                  #         "ttest_p",
                                                                                                  #         "ttest_p_benjaminihochberg_rounded"
                                             point_type = "beeswarm", # "beeswarm", "outliers", NULL
                                             custom_colors = c("white", "gray90", "gray50", "gray30", "gray15"),
                                             bracket_nudge_y = 0.005,
                                             saving_folder = "Boxplot_test",
                                             saving = FALSE, # TRUE to save the plots as images to the saving_folder
                                             group_column = "group", # Column that defines groups that are analyzed
                                             print_significant = FALSE, # Check based on unadjusted wilcox_p
                                             printing = TRUE, # Print figure for viewing
                                             as_object = FALSE, # Assign plot object to global environment
                                             format_value = 3, # Minimum number of decimals to show
                                             round_value = 3, # number of decimals to round to
                                             custom_order = NULL, # (Optional) Manually define the order of groups for plotting
                                             text_size = 12,
                                             plot_title = NULL,
                                             bracket_distance = 0.07, # Corresponds to step.increase in stat_pvalue_manual. Adjusts distance between brackets.
                                             beeswarm_cex = 1.5,
                                             label_stats = FALSE # TRUE to include median in x-axis labels
) {

# Ensure all necessary packages are loaded
required_packages <- c("dplyr","ggplot2","rstatix","ggbeeswarm","here","rlang")

# Check for missing packages
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages), "\n")
  install.packages(missing_packages, repos = "https://cran.r-project.org")
}

require(dplyr)
require(ggplot2)
require(rstatix)
require(ggbeeswarm)
require(here)
require(rlang)

# Subset input data
if (!is.null(columns_to_analyze)) {input_data <- input_data[, c(group_column, columns_to_analyze)]}

# Helper Function 1: Preprocess input data (handles group ordering) #########################################
preprocess_group_data <- function(input_df, group_col_name, custom_group_order) {
  if (!is.null(custom_group_order)) {
    input_df[[group_col_name]] <- factor(input_df[[group_col_name]], levels = custom_group_order)
  } else {
    # Sort alphabetically if no custom order, then factor
    # Ensure consistent ordering for factor creation if data isn't pre-sorted
    ordered_levels <- sort(unique(input_df[[group_col_name]]))
    input_df[[group_col_name]] <- factor(input_df[[group_col_name]], levels = ordered_levels)
  }
  return(input_df)
}

# Helper Function 2: Get variables to plot ##################################################################
get_target_variables <- function(input_df, group_col_name) {
  names(input_df)[!names(input_df) %in% group_col_name]
}

# Helper Function 3: Prepare data subset for a single variable (NA removal, get valid groups) ###############
prepare_variable_analysis_subset <- function(data_df, current_var_name, group_col_name) {
  # Drop rows with NA in the current variable
  subset_df <- data_df[!is.na(data_df[[current_var_name]]), ]
  
  # Get unique group values, respecting factor order from preprocessed data
  # Filter out groups with no data after NA removal for this specific variable
  valid_group_levels <- levels(subset_df[[group_col_name]])
  valid_group_levels <- valid_group_levels[valid_group_levels %in% unique(subset_df[[group_col_name]])]
  
  # Ensure the factor levels in the subset only contain valid groups
  if (length(valid_group_levels) > 0) {
    subset_df[[group_col_name]] <- factor(subset_df[[group_col_name]], levels = valid_group_levels)
  }
  
  return(list(
    data_subset = subset_df,
    group_levels = valid_group_levels
  ))
}

# Helper Function 4: Determine comparison pairs #############################################################
determine_comparison_pairs <- function(group_names_vec, custom_order_exists, user_comparison_list = NULL) {
  if (length(group_names_vec) < 2) {
    return(list())
  }
  
  if (length(group_names_vec) == 2) {
    all_pairs <- list(c(as.character(group_names_vec[1]), as.character(group_names_vec[2])))
  } else {
    all_pairs_matrix <- t(combn(as.character(group_names_vec), 2))
    all_pairs <- split(all_pairs_matrix, seq(nrow(all_pairs_matrix)))
  }
  
  # Sort individual pairs alphabetically if no custom order is provided
  if (!custom_order_exists) {
    all_pairs <- lapply(all_pairs, sort)
  }
  
  if (!is.null(user_comparison_list)) {
    # Also sort user-provided list pairs if no custom order, for matching
    if (!custom_order_exists) {
      user_comparison_list_sorted <- lapply(user_comparison_list, sort)
    } else {
      user_comparison_list_sorted <- user_comparison_list # Keep user's order if custom_order specified overall
    }
    # Filter: keep only pairs that are in the user's list
    # This requires matching list elements, which can be tricky.
    # A robust way is to convert pairs to unique strings.
    pair_to_string <- function(p) paste(sort(p), collapse = " vs ") # Always sort for string comparison key
    
    target_pair_strings <- sapply(user_comparison_list_sorted, pair_to_string)
    all_pairs <- Filter(function(p) pair_to_string(p) %in% target_pair_strings, all_pairs)
    
  }
  
  return(all_pairs)
}

# Helper Function 5: Perform statistical tests and prepare for plotting #####################################
calculate_statistics <- function(data_for_stats, var_name, group_col, comparisons, ordered_group_levels, p_val_round, p_val_format) {
  if (nrow(data_for_stats) == 0 || length(comparisons) == 0) {
    return(data.frame()) # Return empty dataframe if no data or no comparisons
  }
  
  formula_obj <- as.formula(paste0("`", var_name, "` ~ ", group_col))
  
  # Ensure data_for_stats only contains groups that are part of the comparisons
  groups_in_comparisons <- unique(unlist(comparisons))
  stat_data_filtered <- data_for_stats %>% 
    filter(!!sym(group_col) %in% groups_in_comparisons)
  
  if (nrow(stat_data_filtered) < 2 || length(unique(stat_data_filtered[[group_col]])) < 2) {
    return(data.frame()) # Not enough data for tests
  }
  
  stat_test_wilcox <- stat_data_filtered %>%
    wilcox_test(formula_obj, comparisons = comparisons, p.adjust.method = "none", exact=T) # Raw p-values first
  
  if (nrow(stat_test_wilcox) == 0) return(data.frame())
  
  # Add t-test p-values
  ttest_p_values <- numeric(nrow(stat_test_wilcox))
  for (i in 1:nrow(stat_test_wilcox)) {
    g1_name <- stat_test_wilcox$group1[i]
    g2_name <- stat_test_wilcox$group2[i]
    
    group1_data <- stat_data_filtered[stat_data_filtered[[group_col]] == g1_name, var_name, drop = TRUE]
    group2_data <- stat_data_filtered[stat_data_filtered[[group_col]] == g2_name, var_name, drop = TRUE]
    
    if(length(group1_data) >= 2 && length(group2_data) >= 2) { # t.test needs at least 2 obs per group
      ttest_result <- t.test(group1_data, group2_data)
      ttest_p_values[i] <- ttest_result$p.value
    } else {
      ttest_p_values[i] <- NA # Not enough data for t-test
    }
  }
  stat_test_wilcox$ttest_p <- ttest_p_values
  
  # Add y positions for p-value brackets
  # Ensure that 'comparisons' matches the structure expected by add_y_position (list of character vectors)
  # and that groups in comparisons exist in the data passed to add_y_position
  # It's safer to calculate y-positions on the full dataset for the variable
  # or ensure the order of groups in `ordered_group_levels` aligns with `data_for_stats`
  # For add_y_position, data should contain all groups involved in comparisons
  
  # Re-factor data to ensure correct levels for add_y_position
  data_for_ypos <- data_for_stats %>% 
    mutate(!!sym(group_col) := factor(!!sym(group_col), levels=ordered_group_levels))
  
  # Filter comparisons to only those with data
  valid_comparisons_for_ypos <- Filter(function(comp_pair) {
    all(comp_pair %in% unique(as.character(data_for_ypos[[group_col]])))
  }, comparisons)
  
  if (length(valid_comparisons_for_ypos) > 0) {
    stat_test_wilcox <- stat_test_wilcox %>%
      add_y_position(data = data_for_ypos, formula = formula_obj, comparisons = valid_comparisons_for_ypos, step.increase = 0.07) # Using a fixed step.increase, can be parameterized
  } else {
    # if no valid comparisons for y_position (e.g. after filtering)
    # create dummy y.position to avoid errors, though plots might not show brackets
    stat_test_wilcox$y.position <- seq_along(stat_test_wilcox$p) * (max(data_for_stats[[var_name]], na.rm=TRUE) * 0.05) 
  }
  
  # Rename and select columns
  stat_test_wilcox <- stat_test_wilcox %>%
    rename(wilcox_statistic = statistic, wilcox_p = p) %>%
    dplyr::select(-any_of(c(".y.", "p.adj", "p.adj.signif"))) # Remove rstatix adjustments; we do them manually
  
  # Calculate x positions for brackets based on the overall ordered_group_levels
  stat_test_wilcox$xmin <- match(stat_test_wilcox$group1, ordered_group_levels)
  stat_test_wilcox$xmax <- match(stat_test_wilcox$group2, ordered_group_levels)
  
  # P-value adjustments and formatting
  stat_test_wilcox$wilcox_p_rounded <- format(round(stat_test_wilcox$wilcox_p, digits = p_val_round), nsmall = p_val_format)
  stat_test_wilcox$wilcox_p_holmbonferroni <- p.adjust(stat_test_wilcox$wilcox_p, method = "holm")
  stat_test_wilcox$wilcox_p_benjaminihochberg <- p.adjust(stat_test_wilcox$wilcox_p, method = "BH")
  stat_test_wilcox$wilcox_p_benjaminihochberg_rounded <- format(round(stat_test_wilcox$wilcox_p_benjaminihochberg, digits = p_val_round), nsmall = p_val_format)
  stat_test_wilcox$wilcox_p_benjaminihochberg_rounded[stat_test_wilcox$wilcox_p_benjaminihochberg < (1/(10^p_val_format)) & !is.na(stat_test_wilcox$wilcox_p_benjaminihochberg)] <- paste0("<0.", paste(rep("0",p_val_format-1), collapse=""),"1")
  
  
  stat_test_wilcox$ttest_p_rounded <- format(round(stat_test_wilcox$ttest_p, digits = p_val_round), nsmall = p_val_format)
  stat_test_wilcox$ttest_p_benjaminihochberg <- p.adjust(stat_test_wilcox$ttest_p, method = "BH")
  stat_test_wilcox$ttest_p_benjaminihochberg_rounded <- format(round(stat_test_wilcox$ttest_p_benjaminihochberg, digits = p_val_round), nsmall = p_val_format)
  stat_test_wilcox$ttest_p_benjaminihochberg_rounded[stat_test_wilcox$ttest_p_benjaminihochberg < (1/(10^p_val_format)) & !is.na(stat_test_wilcox$ttest_p_benjaminihochberg)] <- paste0("<0.", paste(rep("0",p_val_format-1), collapse=""),"1")
  
  return(stat_test_wilcox)
}

# Helper Function 6: Prepare plot annotations (N, medians, labels) ##########################################
prepare_plot_annotations <- function(data_for_plot, var_name, group_col, ordered_group_levels, show_median_in_label, median_round_digits) {
  # Calculate N for each group
  group_counts <- data_for_plot %>%
    group_by(!!sym(group_col)) %>%
    summarize(n = n(), .groups = 'drop')
  
  # Calculate medians
  group_medians <- data_for_plot %>%
    group_by(!!sym(group_col)) %>%
    summarize(median_val = median(.data[[var_name]], na.rm = TRUE), .groups = 'drop')
  
  # Create labels for x-axis ticks
  # Ensure ordered_group_levels are characters for matching
  char_ordered_group_levels <- as.character(ordered_group_levels)
  
  axis_labels <- sapply(char_ordered_group_levels, function(g_level) {
    n_val <- group_counts$n[match(g_level, as.character(group_counts[[group_col]]))]
    base_label <- paste0(g_level, "\nn=", ifelse(is.na(n_val), 0, n_val))
    if (show_median_in_label) {
      median_val <- group_medians$median_val[match(g_level, as.character(group_medians[[group_col]]))]
      formatted_median <- format(round(median_val, digits = median_round_digits), nsmall = median_round_digits)
      base_label <- paste0(g_level, "\nmedian ", formatted_median, "\nn=", ifelse(is.na(n_val), 0, n_val))
    }
    base_label
  }, USE.NAMES = FALSE) # USE.NAMES = FALSE if you want an unnamed vector
  
  names(axis_labels) <- char_ordered_group_levels # ggplot can use named vector for labels
  
  # Data for plotting (just add current variable values as a consistent column name)
  plot_df <- data_for_plot
  plot_df$current_var_for_plot <- plot_df[[var_name]]
  
  return(list(
    axis_tick_labels = axis_labels,
    plotting_data = plot_df
  ))
}

# Helper Function 7: Build the ggplot boxplot ###############################################################
build_boxplot_gg <- function(data_to_plot, group_col, axis_labels, stat_results,
                             y_axis_label_main, x_axis_label_text, p_value_display_col,
                             y_bracket_nudge, font_size, p_bracket_spacing, swarm_dot_size, num_defined_groups) {
  
  plot_obj <- ggplot(data = data_to_plot, aes(x = !!sym(group_col), y = current_var_for_plot, fill = !!sym(group_col)))
  
  if (is.null(point_type)) { # if set to null, no outliers or beeswarm
  plot_obj <- plot_obj + geom_boxplot(linewidth = 1, fatten = 1, width = 0.6, outliers = FALSE, color = "black")
  } else if (point_type == "beeswarm") { # beeswarm draws both outliers and other points
      plot_obj <- plot_obj + geom_boxplot(linewidth = 1, fatten = 1, width = 0.6, outliers = FALSE, color = "black") +
                           geom_beeswarm(cex = swarm_dot_size, na.rm = TRUE) # na.rm for safety
  } else if (point_type == "outlier") { # outliers drawn separately without beeswamr
      plot_obj <- plot_obj + geom_boxplot(linewidth = 1, fatten = 1, width = 0.6, outliers = TRUE, color = "black")
  } else { 
    plot_obj <- plot_obj + geom_boxplot(linewidth = 1, fatten = 1, width = 0.6, outliers = FALSE, color = "black")
    } # if set to something else than null, beeswarm or outliers, behaves as if set to null
    
  plot_obj <- plot_obj + theme_bw() +
    xlab(x_axis_label_text) +
    ylab(y_axis_label_main) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) + # Increased upper expansion
    scale_x_discrete(labels = axis_labels) +
    theme(
      text = element_text(color = "black"),
      axis.title = element_text(size = font_size),
      axis.text = element_text(size = font_size, color = "black"),
      line = element_line(color = "black"),
      axis.line = element_line(color = "black"),
      panel.grid = element_blank(),
      panel.border = element_rect(linewidth = 1.5, color = "black"),
      axis.ticks = element_line(linewidth = 1.5, color = "black"),
      legend.position = "none"
    ) +
    scale_fill_manual(values = custom_colors[1:num_defined_groups]) # Ensure enough colors
  
  if (!is.null(plot_title)) { # custom plot title
    plot_obj <- plot_obj + ggtitle(plot_title)
  }
  
  if (nrow(stat_results) > 0 && p_value_display_col %in% names(stat_results)) {
    plot_obj <- plot_obj +
      stat_pvalue_manual(data = stat_results,
                         label = p_value_display_col,
                         xmin = "xmin",
                         xmax = "xmax",
                         y.position = "y.position",
                         tip.length = 0.01,
                         bracket.size = 1,
                         step.increase = p_bracket_spacing,
                         bracket.nudge.y = y_bracket_nudge,
                         color = "black",
                         size = font_size / .pt, # Convert pt to mm for size aesthetic
                         inherit.aes = FALSE)
  }
  return(plot_obj)
}

# Helper Function 8: Manage plot output (print, save, assign to global) #####################################
manage_plot_output <- function(gg_plot, var_name_cleaned, stat_df,
                               assign_to_global, print_plot, only_print_significant,
                               significance_check_col, # e.g. "wilcox_p"
                               save_plot, plot_save_folder,
                               plot_width_px = 1800, plot_height_px = 1800) {
  
  if (assign_to_global) {
    assign(paste0("plot_temp_", gsub("[^[:alnum:]_]", "", var_name_cleaned)), gg_plot, envir = .GlobalEnv)
  }
  
  should_print_this_plot <- FALSE
  if (print_plot) {
    if (only_print_significant) {
      if (nrow(stat_df) > 0 && significance_check_col %in% names(stat_df) && any(stat_df[[significance_check_col]] <= 0.05, na.rm = TRUE)) {
        should_print_this_plot <- TRUE
      }
    } else {
      should_print_this_plot <- TRUE
    }
  }
  
  if (should_print_this_plot) {
    print(gg_plot)
  }
  
  if (save_plot) {
    if (!dir.exists(plot_save_folder)) {
      dir.create(plot_save_folder, recursive = TRUE)
    }
    # Clean variable name for filename
    filename_var_part <- gsub("[ \\(\\)\\.\\%\\/\\*\\-]", "_", var_name_cleaned) # Replace common problematic chars
    filename_var_part <- gsub("__+", "_", filename_var_part) # Replace multiple underscores
    filename <- sprintf("%s.svg", paste0("boxplot_", filename_var_part))
    
    # Use tryCatch for saving in case of issues
    tryCatch({
      ggsave(filename = here(plot_save_folder, filename), plot = gg_plot, units = "px", width = plot_width_px, height = plot_height_px)
    }, error = function(e) {
      warning(paste("Failed to save plot for variable:", var_name_cleaned, "Error:", e$message))
    })
  }
}

# MAIN FUNCTION #############################################################################################
  # Step 1. Preprocess group data (factor conversion and ordering)
  processed_data <- preprocess_group_data(input_data, group_column, custom_order)
  
  # Step 2. Get target variables for plotting
  variables_to_plot <- get_target_variables(processed_data, group_column)
  
  all_test_results_list <- list() # To accumulate results from all variables
  
  for (current_variable in variables_to_plot) {
    cat("Processing variable:", current_variable, "\n")
    
    # Step 3. Prepare data subset for the current variable
    analysis_subset_info <- prepare_variable_analysis_subset(processed_data, current_variable, group_column)
    variable_data_subset <- analysis_subset_info$data_subset
    current_variable_group_levels <- analysis_subset_info$group_levels
    
    if (nrow(variable_data_subset) == 0 || length(current_variable_group_levels) < 2) {
      cat("  Skipping variable", current_variable, "due to insufficient data or groups after NA removal.\n")
      next
    }
    
    # Step 4. Determine comparison pairs
    # Note: custom_order being NULL means !is.null(custom_order) is FALSE
    comparisons_for_variable <- determine_comparison_pairs(current_variable_group_levels, 
                                                           custom_order_exists = !is.null(custom_order), 
                                                           user_comparison_list = comparison_list)
    
    if (length(comparisons_for_variable) == 0 && !is.null(comparison_list)) {
      cat("  No specified comparisons are valid for variable", current_variable, "with current group levels. Skipping p-value annotations.\n")
    } else if (length(comparisons_for_variable) == 0 && length(current_variable_group_levels) >=2 ) {
      # This case might happen if determine_comparison_pairs returns empty for other reasons (e.g. <2 groups, though checked above)
      # or if user_comparison_list leads to no valid pairs.
      cat("  No comparison pairs generated for variable", current_variable, ". P-values will not be displayed.\n")
    }
    
    
    # Step 5. Calculate statistical tests
    # Ensure `current_variable_group_levels` is used for ordering in stats (esp. xmin/xmax)
    stat_results <- calculate_statistics(variable_data_subset, current_variable, group_column,
                                         comparisons_for_variable, current_variable_group_levels,
                                         round_value, format_value)
    
    # Add current variable name to results for aggregation
    if (nrow(stat_results) > 0) {
      stat_results$variable <- current_variable
      all_test_results_list[[current_variable]] <- stat_results
    } else {
      cat("  No statistical results generated for variable", current_variable, " (possibly due to insufficient data in comparison groups).\n")
    }
    
    # Step 6. Prepare plot annotations (axis labels, etc.)
    # The median is rounded to 2 decimal places in the original code for labels, let's make it a parameter if needed or keep fixed.
    plot_annotations <- prepare_plot_annotations(variable_data_subset, current_variable, group_column,
                                                 current_variable_group_levels, label_stats, median_round_digits = 2)
    
    y_axis_title <- paste0(current_variable, y_unit)
    
    # Step 7. Build the ggplot boxplot
    # Ensure p_value_type column exists in stat_results. If not, default to a safe one or skip p-values.
    p_val_col_to_display <- p_value_type
    if (nrow(stat_results) > 0 && !p_value_type %in% names(stat_results)) {
      warning(paste0("Specified p_value_type '", p_value_type, "' not found in results for variable '", current_variable, "'. Defaulting to 'wilcox_p' or no p-values if that also fails."))
      if ("wilcox_p" %in% names(stat_results)) {
        p_val_col_to_display <- "wilcox_p"
      } else {
        # Fallback if even wilcox_p is not there (should not happen if stat_results is populated)
        p_val_col_to_display <- "" # Effectively skips p-value display if column is empty string
      }
    } else if (nrow(stat_results) == 0) {
      p_val_col_to_display <- "" # No stats, no p-value display
    }
    
    
    boxplot_gg <- build_boxplot_gg(plot_annotations$plotting_data, group_column, 
                                   plot_annotations$axis_tick_labels, stat_results,
                                   y_axis_title, x_unit, p_val_col_to_display,
                                   bracket_nudge_y, text_size, bracket_distance, 
                                   beeswarm_cex, length(current_variable_group_levels))
    
    # Step 8. Manage plot output
    # For print_significant, the original code checks 'wilcox_p'. Let's stick to that for the condition.
    manage_plot_output(boxplot_gg, current_variable, stat_results,
                       as_object, printing, print_significant, "wilcox_p", # significance_check_col
                       saving, saving_folder)
  } # End loop over variables
  
  # Combine all test results into a single data frame
  if (length(all_test_results_list) > 0) {
    final_results_df <- dplyr::bind_rows(all_test_results_list)
    return(final_results_df)
  } else {
    cat("No test results were generated for any variable.\n")
    return(NULL)
  }
} # End main function

