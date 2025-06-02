#' @title generate_demographic_tables
#' @description
#' \code{\link{generate_demographic_tables}} This function creates demographic summary 
#' tables and calculates statistical tests between groups.
#'
#' @param input_data data.frame containing the variables to be analyzed.
#' @param parameter_list Vector of column names to include in the analysis.
#' @param group_column Character string specifying the grouping variable.
#' @param sex_column Character string specifying the sex variable (optional).
#' @param female_value Value used to identify females in sex_column.
#' @param male_value Value used to identify males in sex_column.
#' @param format_value Number of decimal places for formatting rounded values.
#' @param completed_table_statistic Which statistic to show in the final table. 
#'        Options: 
#'        "min_max", 
#'        "mean", 
#'        "sd", 
#'        "median", 
#'        "min", 
#'        "max".
#' @param completed_table_sex_statistic Which sex statistic to show in the final table.
#'        Options: 
#'        "n_m_f" n males and n females
#'        "n_fn_ratio" n with female n to n ratio, 
#'        "n_f_fn_ratio", n female with n female to n ratio,
#'        "f_n_ratio", n female to n ratio
#'        "n_f", n females
#'        "n_m" n males
#' @param freeform_brackets (Optional) Add another of statistic in brackets after completed_table_statistic, 
#' eg. choose median in completed_table_statistic and choose Q1-Q3 here to get "median (Q1-Q3)"
#'         Options:
#'         "sd",
#'         "Q1_Q3",
#' @param multiple_comparison_correction Method for correcting p-values. 
#'        Options: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", NULL.
#' @param comparison_list Optional list of specific group comparisons to make.
#' 
#' @name generate_demographic_tables
#'
#' @return
#'  \item{demographic_statistics}{Raw demographic statistics for all groups}
#'  \item{sex_ratios}{Sex distribution statistics by group}
#'  \item{complete_table}{Final formatted table suitable for publication}
#'  
#' @examples
#' \dontrun{
#' # Using the function
#' results <- generate_demographic_tables()
#' print(results$demographic_statistics)
#' print(results$group_wise_comparisons)
#' print(results$sex_ratios)
#' print(results$complete_table)
#' 
#' }                             
#'
#' @export
generate_demographic_tables <- function(
    input_data = mtcars,                           # Input data frame or tibble
    parameter_list = c("mpg", "disp", "qsec"),     # Columns to analyze. Rows in output will appear in this order, though n and sex ratios will be the top rows.
    group_column = "am",                           # Column containing groups to compare. No group defined in this column can be all NA!
    completed_table_statistic = "median",          # Descriptive statistic to show in final table
    freeform_brackets = "Q1_Q3",                   # (Optional) Statistic to show in brackets after main statistic
    groupwise_test = "wilcox",                     # Statistical test to use for group-wise comparisons ("wilcox" or "ttest")
    multiple_comparison_correction = "BH",         # (Optional) P-value multiple comparison correction method (NULL to skip)
    comparison_list = NULL,                        # (Optional) List of comparisons. Restricts the number of group wise comparisons and multiple comparison correction.
                                                   # eg. list(c("column1", "column2"),c("column1","column3")). Use this if, for example, you want to compare all MS to HC and MS subgroups 
                                                   # to HC, but not MS subgroups to all MS. You need to create the dataframe with these overlapping groups in one column separately.
    multiple_comparison_correction_brackets = FALSE, # (Optional) include corrected group-wise comparisons value in brackets after main p-value
    groupwise_95CI = FALSE,                        # (Optional) include 95% CI from group-wise comparison test in square brackets after comparison p-value. These are naive and not adjusted.
    sex_column = "vs",                             # (optional) Categorical sex variable to calculate sex ratio
    female_value = 0,                              # (optional, but required with sex_column) Female identifier
    male_value = 1,                                # (optional, but required with sex_column) Male identifier
    completed_table_sex_statistic = "n_m_f",       # (Optional, but required with sex_column) Sex statistic for final table, eg. sex ratio
    group_n = TRUE,                                # Include group n in the formatted output table
    format_value = 2,                              # Minimum number of decimals to show
    p_format_value = 3,                            # Minimum number of decimals to show for p-values
    mean_if_normal = FALSE,                        # Use mean and sd if parameter is normally distributed
    ttest_if_normal = FALSE,                       # Use t-test if parameter is normally distributed
    ordinal_columns = NULL                       # List of ordinal columns. Median and wilcox test are shown for these regardless of their distribution.
) {
  
  # Dependencies
  required_packages <- c("tibble", "tidyr", "dplyr")
  
  # Check for missing packages
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  if(length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages), "\n")
    install.packages(missing_packages, repos = "https://cran.r-project.org")
  }
  
  require(tibble)
  require(tidyr)
  require(dplyr)
  
  # 0. ensure input is appropriate ####
  text_output <- "" # This string records information about settings and exclusions
  
  # Convert to data frame. In case its a tibble
  input_data <- as.data.frame(input_data)
  
  # Check that group column is in input data
  if (!(group_column %in% names(input_data))) {
    stop(paste0("Specified group column '", group_column, "' is not present in the input data."))
  }
  
  # Check that sex column is in input data
  if (!is.null(sex_column)) {if (!( sex_column %in% names(input_data))) {
    text_output <- paste0(text_output,"\n\nSex column '", sex_column, "' is not present in the input data and sex statistic was not calculated.")
    sex_column <- NULL
  } }
  
  # Make sure ordinal columns are in parameter list
  if (!is.null(ordinal_columns)) {if (!(ordinal_columns %in% parameter_list)) {
    parameter_list <- c(parameter_list,ordinal_columns)
  }}
  
  # Filter to keep only appropriate columns and update parameter_list accordingly
  numeric_cols <- names(input_data)[sapply(input_data, is.numeric)]
  essential_cols <- c(sex_column, group_column)
  essential_cols <- essential_cols[!is.null(essential_cols)]
  keep_cols <- unique(c(essential_cols, numeric_cols))
  missing_columns <- parameter_list[!parameter_list %in% names(input_data)] # Check what's being excluded from parameter_list and why
  non_numeric_columns <- parameter_list[parameter_list %in% names(input_data) & 
                                          !parameter_list %in% numeric_cols &
                                          !parameter_list %in% essential_cols]
  parameter_list <- parameter_list[parameter_list %in% keep_cols] # Update parameter_list to only include valid columns
  input_data <- input_data[, keep_cols, drop = FALSE] # Filter input_data
  if (length(missing_columns) > 0) { # Report exclusions
    text_output <- paste0(text_output,"\n\nSpecified parameter(s) '",paste(missing_columns, collapse = "', '"),"' are not column names in the input data and were excluded.")
  }
  if (length(non_numeric_columns) > 0) { # Report exclusions
    text_output <- paste0(text_output,"\n\nNon-numeric parameter(s) '",paste(non_numeric_columns, collapse = "', '"),"' were excluded from statistics calculations.")
  }
  
  # Make sure sex column is not in parameter list
  parameter_list <- parameter_list[!parameter_list %in% sex_column]
  
  
  # 1. Collect all possible group comparisons ####
  group_values <- unique(input_data[[group_column]])
  
  if (length(group_values) > 2) {
    all_comparisons <- t(combn(as.character(group_values), 2))
    all_comparisons <- split(all_comparisons, seq(nrow(all_comparisons)))
  } else {
    # Handle case with only 2 groups
    all_comparisons <- list(c(group_values[1], group_values[2]))
  }
  
  ## Sort each comparison pair alphabetically ####
  all_comparisons <- lapply(all_comparisons, sort)
  
  ## Use specific comparisons if provided ####
  if(!is.null(comparison_list)) {
    comparison_list <- lapply(comparison_list, sort)
    all_comparisons <- all_comparisons[all_comparisons %in% comparison_list]
  }
  
  # 2. Calculate descriptive statistics ####
  
  table_stats <- data.frame()
  
  for (loop_group in unique(input_data[[group_column]])) {
    for (loop_column in parameter_list) {
      collection <- input_data[input_data[[group_column]] == loop_group, ]
      
     ## Calculate descriptive statistic for current parameter and group ####
      temp <- data.frame(
        parameter = loop_column,
        group = loop_group,
        n = nrow(collection),
        mean = format(round(mean(collection[[loop_column]], na.rm = TRUE),format_value),nsmall = format_value),
        sd = format(round(sd(collection[[loop_column]], na.rm = TRUE),format_value),nsmall = format_value),
        
        median = format(round(median(collection[[loop_column]], na.rm = TRUE),format_value),nsmall = format_value),
        Q1 = format(round(quantile(collection[[loop_column]], na.rm = TRUE)[2],format_value),nsmall = format_value),
        Q3 = format(round(quantile(collection[[loop_column]], na.rm = TRUE)[4],format_value),nsmall = format_value),
        min = format(round(min(collection[[loop_column]], na.rm = TRUE),format_value),nsmall = format_value),
        max = format(round(max(collection[[loop_column]], na.rm = TRUE),format_value),nsmall = format_value),
        stringsAsFactors = FALSE
      )
      
      # Format combined statistics
      temp$Q1_Q3 <- paste0(temp$Q1," - ",temp$Q3)
      temp$min_max <- paste0(temp$min," - ",temp$max)
      
      ## Freeform statistic, this is the formatted statistic with range or sd etc. in brackets ####
      # Override for ordinal columns - always use median (Q1-Q3)
      if (loop_column %in% ordinal_columns) {
        temp$freeform <- paste0(temp$median, " (", temp$Q1_Q3, ")")
      } else {
        temp$freeform <- temp[[completed_table_statistic]]
        if(!is.null(freeform_brackets)) {
          temp$freeform <- paste0(temp$freeform," (",temp[[freeform_brackets]],")")
        }
      }
      
      ## Shapiro-Wilk test for normality (if appropriate) ####
      if (sum(!is.na(collection[[loop_column]])) > 2 && 
          sum(!is.na(collection[[loop_column]])) < 5001) {
        temp$shapiro_p <- format(round(
          shapiro.test(collection[[loop_column]])$p.value, p_format_value), nsmall = p_format_value)
        temp$normality <- ifelse(shapiro.test(collection[[loop_column]])$p.value < 0.05,
                                 "Not normal", "Normal")
      } else {
        temp$shapiro_p <- NA
        temp$normality <- NA
      }
      
      # Append to results
      table_stats <- rbind(table_stats, temp)
    }
  }
  
  # 3. Calculate sex statistics if sex column is provided ####
  
  if (!is.null(sex_column)) {
    table_sex <- data.frame()
    
    for (loop_group in unique(input_data[[group_column]])) {
      collection <- input_data[input_data[[group_column]] == loop_group, ]
      
      temp <- data.frame(
        group = loop_group,
        n = nrow(collection),
        n_f = sum(collection[[sex_column]] == female_value),
        n_m = sum(collection[[sex_column]] == male_value),
        stringsAsFactors = FALSE
      )
      
      # Calculate ratios and combined statistics
      temp$n_m_f <- paste0(temp$n_m, "/", temp$n_f)
      temp$f_n_ratio <- format(round(temp$n_f/temp$n * 100, 0), 
                               nsmall = 0)
      temp$f_n_ratio <- paste0(temp$f_n_ratio," %")
      temp$n_fn_ratio <- paste0(temp$n, " (", temp$f_n_ratio, ")")
      temp$n_f_fn_ratio <- paste0(temp$n_f, " (", temp$f_n_ratio, ")")
      
      table_sex <- rbind(table_sex, temp)
    }
    
    ## Fisher's exact test for sex distribution ####
    fisher_results <- data.frame(matrix(ncol = length(all_comparisons), nrow = 1))
    
    for (loop_comparison in 1:length(all_comparisons)) {
      variable1 = all_comparisons[[loop_comparison]][1]
      variable2 = all_comparisons[[loop_comparison]][2]
      
      # Create contingency table
      temp_contingency <- matrix(c(
        table_sex$n_m[table_sex$group == variable1],
        table_sex$n_m[table_sex$group == variable2],
        table_sex$n_f[table_sex$group == variable1],
        table_sex$n_f[table_sex$group == variable2]
      ), nrow = 2)
      
      names(fisher_results)[loop_comparison] <- paste0(variable1, " vs ", variable2)
      fisher_results[1, loop_comparison] <- format(round(
        as.numeric(fisher.test(temp_contingency)["p.value"]), p_format_value), nsmall = p_format_value)
    }
    
    # Reshape sex table for output
    table_sex <- rownames_to_column(as.data.frame(t(table_sex)), "parameter")
    names(table_sex) <- table_sex[1, ]
    names(table_sex)[1] <- "parameter"
    table_sex <- table_sex[table_sex$parameter != "n", ]
    table_sex <- cbind(table_sex, fisher_results)
  } else {
    table_sex <- NA
  }
  
  # 4. Group wise comparisons Wilcox and t-test for each parameter and comparison ####
  
  wilcox_results <- data.frame()
  ttest_results <- data.frame()
  
  for (loop_parameter in parameter_list) {
    temp_results_wilcox <- data.frame(parameter = loop_parameter, stringsAsFactors = FALSE)
    temp_results_ttest <- data.frame(parameter = loop_parameter, stringsAsFactors = FALSE)
    
    for (loop_comparison in 1:length(all_comparisons)) {
      variable1 = all_comparisons[[loop_comparison]][1]
      variable2 = all_comparisons[[loop_comparison]][2]
      
      # Extract data for the two groups
      data1 <- na.omit(input_data[input_data[[group_column]] == variable1, loop_parameter])
      data2 <- na.omit(input_data[input_data[[group_column]] == variable2, loop_parameter])
      
      # Create column names for comparisons
      comparison_name <- paste0(variable1, " vs ", variable2)
      comparison_name_95CI <- paste0(comparison_name," 95CI")
      
      # Perform tests if both groups have data
      if (length(data1) > 0 && length(data2) > 0) {
        
        # wilcox
        wilcox <- wilcox.test(data1, data2, conf.int = TRUE)
        wilcox_p <- wilcox$p.value
        wilcox_95CI <- paste0( format(round(wilcox$conf.int[1],format_value),nsmall = format_value),
                               " - ",
                               format(round(wilcox$conf.int[2],format_value),nsmall = format_value) 
        )
        temp_results_wilcox[1, comparison_name] <- wilcox_p
        temp_results_wilcox[1, comparison_name_95CI] <- wilcox_95CI
        
        # ttest
        ttest <- t.test(data1, data2)
        ttest_p <- ttest$p.value
        ttest_95CI <- paste0( format(round(ttest$conf.int[1],format_value),nsmall = format_value),
                              " - ",
                              format(round(ttest$conf.int[2],format_value),nsmall = format_value) 
        )
        temp_results_ttest[1, comparison_name] <- ttest_p
        temp_results_ttest[1, comparison_name_95CI] <- ttest_95CI
        
      } else {
        temp_results_wilcox[1, comparison_name] <- NA
        temp_results_ttest[1, comparison_name] <- NA
        temp_results_wilcox[1, comparison_name_95CI] <- NA
        temp_results_ttest[1, comparison_name_95CI] <- NA
      }
    }
    
    wilcox_results <- rbind(wilcox_results, temp_results_wilcox)
    ttest_results <- rbind(ttest_results, temp_results_ttest)
  }
  
  ## Apply multiple comparison correction ####
  wilcox_results_corrected <- wilcox_results
  ttest_results_corrected <- ttest_results
  
  col_names <- names(wilcox_results)
  p_value_columns <- col_names[grepl(" vs ", col_names) & !grepl("95CI", col_names)] # Only p-value columns
  
  for (loop_row in 1:nrow(wilcox_results_corrected)) {
    wilcox_results_corrected[loop_row, p_value_columns] <- p.adjust(
      p = as.numeric(unlist(wilcox_results_corrected[loop_row, p_value_columns])), 
      method = multiple_comparison_correction
    )
    ttest_results_corrected[loop_row, p_value_columns] <- p.adjust(
      p = as.numeric(unlist(ttest_results_corrected[loop_row, p_value_columns])), 
      method = multiple_comparison_correction
    )
  }
  
  ## Format p-values using sprintf for consistent formatting ####
  format_p_values <- function(p_vals, decimals) { # helper function
    formatted <- ifelse(p_vals < 0.001, "<0.001", sprintf(paste0("%.", decimals, "f"), p_vals))
    return(formatted)
  }
  
  for(col in p_value_columns) {
    wilcox_results[, col] <- format_p_values(wilcox_results[, col], p_format_value)
    ttest_results[, col] <- format_p_values(ttest_results[, col], p_format_value)
    wilcox_results_corrected[, col] <- format_p_values(wilcox_results_corrected[, col], p_format_value)
    ttest_results_corrected[, col] <- format_p_values(ttest_results_corrected[, col], p_format_value)
  }
  
  
  # 5. Create final formatted table ####
  wide_table <- pivot_wider( # Whack the silly columns sideways
    table_stats[c("parameter", "group", "freeform")],
    names_from = group,
    values_from = freeform
  )
  
  ## Determine parameter-level normality (ALL groups must be normal for parameter to be considered normal) ####
  parameter_normality <- data.frame()
  for (loop_parameter in parameter_list) {
    all_groups_normal <- TRUE
    
    for (loop_group in unique(input_data[[group_column]])) {
      normality_status <- table_stats$normality[table_stats$parameter == loop_parameter & table_stats$group == loop_group]
      if (is.na(normality_status) || normality_status != "Normal") {
        all_groups_normal <- FALSE
        break
      }
    }
    
    parameter_normality <- rbind(parameter_normality, data.frame(
      parameter = loop_parameter,
      all_groups_normal = all_groups_normal,
      stringsAsFactors = FALSE
    ))
  }
  
  ## Update freeform statistics based on parameter-level normality ####
  if (mean_if_normal == TRUE) {
    for (i in 1:nrow(table_stats)) {
      param <- table_stats$parameter[i]
      # Skip ordinal columns - they should always use median (Q1-Q3)
      if (!(param %in% ordinal_columns) && 
          parameter_normality$all_groups_normal[parameter_normality$parameter == param]) {
        table_stats$freeform[i] <- paste0(table_stats$mean[i], " (", table_stats$sd[i], ")")
      }
    }
  }
  
  ## Recreate wide_table after potential freeform changes ####
  wide_table <- pivot_wider(
    table_stats[c("parameter", "group", "freeform")],
    names_from = group,
    values_from = freeform
  )
  
  ## Determine which group-wise test results to use for main table ####
  if (multiple_comparison_correction_brackets == TRUE) {
    results_for_main <- if(groupwise_test == "wilcox") wilcox_results else ttest_results
  } else if (is.null(multiple_comparison_correction)) {
    results_for_main <- if(groupwise_test == "wilcox") wilcox_results else ttest_results
  } else {
    results_for_main <- if(groupwise_test == "wilcox") wilcox_results_corrected else ttest_results_corrected
  }
  
  ## Create ordinal column rows ####
  if (!is.null(ordinal_columns)) {
   # re-create group wise results table with appropriate data for ordinal columns
    if (multiple_comparison_correction_brackets == TRUE) {
      wilcox_for_ordinals <- wilcox_results
    } else if (is.null(multiple_comparison_correction)) {
      wilcox_for_ordinals <- wilcox_results
    } else {
      wilcox_for_ordinals <- wilcox_results_corrected
    }
    
    # Replace ordinal parameter rows with wilcox results
    for (loop_ordinal in ordinal_columns) { #in the beginning of code it is ensured that this column is in parameter list and thus in results_for_main
      results_for_main[results_for_main$parameter == loop_ordinal,] <- wilcox_for_ordinals[wilcox_for_ordinals$parameter == loop_ordinal,]
    }
  }
  
  
  ## Merge group-wise comparison results with wide table (sort = FALSE to preserve order) ####
  table_final <- merge(wide_table, results_for_main, by = "parameter", all.x = TRUE, sort = FALSE)
  
  
  ## Replace with t-test results if requested and parameter is normal across ALL groups ####
  if (ttest_if_normal == TRUE) {
    p_columns <- names(table_final)[grepl(" vs ", names(table_final)) & !grepl("95CI", names(table_final))]
    ci_columns <- names(table_final)[grepl(" vs ", names(table_final)) & grepl("95CI", names(table_final))]
    
    for (loop_parameter in table_final$parameter) {
      # Check if ALL groups are normal for this parameter AND it's not an ordinal column
      if (!(loop_parameter %in% ordinal_columns) && 
          parameter_normality$all_groups_normal[parameter_normality$parameter == loop_parameter]) {
        
        ### Replace p-values with t-test results ####
        for (loop_comparison in p_columns) {
          if (!is.null(multiple_comparison_correction) & multiple_comparison_correction_brackets == FALSE) {
            # Use corrected t-test p-value as main value
            table_final[[loop_comparison]][table_final$parameter == loop_parameter] <- 
              ttest_results_corrected[[loop_comparison]][ttest_results_corrected$parameter == loop_parameter]
          } else {
            # Use uncorrected t-test p-value as main value
            table_final[[loop_comparison]][table_final$parameter == loop_parameter] <- 
              ttest_results[[loop_comparison]][ttest_results$parameter == loop_parameter]
          }
        }
        
        ### Replace CIs with t-test CIs - use consistent source with p-values ####
        
        if (loop_ci_column %in% names(table_final)) {
          # Use same correction status as p-values for consistency
          if (!is.null(multiple_comparison_correction) & multiple_comparison_correction_brackets == FALSE) {
            # Use corrected t-test CIs to match corrected p-values
            table_final[[loop_ci_column]][table_final$parameter == loop_parameter] <- 
              ttest_results_corrected[[loop_ci_column]][ttest_results_corrected$parameter == loop_parameter]
          } else {
            # Use uncorrected t-test CIs to match uncorrected p-values  
            table_final[[loop_ci_column]][table_final$parameter == loop_parameter] <- 
              ttest_results[[loop_ci_column]][ttest_results$parameter == loop_parameter]
          }
        }
      }
    }
  }
  
  ## Handle 95% CI display (keep or combine with p-values) ####
  if (groupwise_95CI == FALSE) {
    # Remove CI columns if not requested
    table_final <- table_final[, -grep("95CI", names(table_final)), drop = FALSE]
  } else {
    # Combine CI with p-values in brackets
    ci_columns <- grep("95CI", names(table_final), value = TRUE)
    
    for (loop_ci_column in ci_columns) {
      p_col <- gsub(" 95CI$", "", loop_ci_column)
      # Combine p-value and CI
      table_final[[p_col]] <- paste0(table_final[[p_col]], " [", table_final[[loop_ci_column]], "]")
    }
    # Remove separate CI columns
    table_final <- table_final[, -grep("95CI", names(table_final)), drop = FALSE]
  }
  
  ## Add corrected p-values in brackets if requested ####
  if (multiple_comparison_correction_brackets == TRUE & !is.null(multiple_comparison_correction)) {
    p_columns <- grep(" vs ", names(table_final), value = TRUE)
    
    for (loop_p_column in p_columns) {
      for (loop_parameter in table_final$parameter) {
        # Determine which corrected results to use based on parameter normality
        if (ttest_if_normal == TRUE && 
            !(loop_parameter %in% ordinal_columns) &&
            parameter_normality$all_groups_normal[parameter_normality$parameter == loop_parameter]) {
          # Use corrected t-test results
          corrected_p <- ttest_results_corrected[[loop_p_column]][ttest_results_corrected$parameter == loop_parameter]
        } else {
          # Use corrected Wilcox results
          corrected_p <- wilcox_results_corrected[[loop_p_column]][wilcox_results_corrected$parameter == loop_parameter]
        }
        
        # Add corrected p-value in parentheses
        current_value <- table_final[[loop_p_column]][table_final$parameter == loop_parameter]
        table_final[[loop_p_column]][table_final$parameter == loop_parameter] <- 
          paste0(current_value, " (", corrected_p, ")")
      }
    }
  }
  
  
  
  ## Combine all group wise comparisons into one table. Notice that the column names change here ####
  names(wilcox_results)[-1] <- paste0("wilcox ",names(wilcox_results)[-1])
  names(wilcox_results_corrected)[-1] <- paste0("wilcox corrected ",names(wilcox_results_corrected)[-1])
  names(ttest_results)[-1] <- paste0("ttest ",names(ttest_results)[-1])
  names(ttest_results_corrected)[-1] <- paste0("ttest corrected ",names(ttest_results_corrected)[-1])
  
  table_groupwise <- merge(ttest_results, wilcox_results, by = "parameter", all.x = TRUE, sort = FALSE)
  table_groupwise <- merge(table_groupwise, wilcox_results_corrected, by = "parameter", all.x = TRUE, sort = FALSE)
  table_groupwise <- merge(table_groupwise, ttest_results_corrected, by = "parameter", all.x = TRUE, sort = FALSE)
  
  ## Reorder final table rows to match the order of parameter_list ####
  table_final$parameter <- factor(table_final$parameter, levels = parameter_list)
  table_final <- table_final[order(table_final$parameter), ]
  table_final$parameter <- as.character(table_final$parameter)  # Convert back to character for consistent handling
  
  ## Add sex ratios if sex column is defined ####
  if (!is.null(sex_column)) {
    sex_row <- table_sex[table_sex$parameter == completed_table_sex_statistic, ]
    table_final <- rbind(sex_row, table_final)
  }
  
  ## Add group sizes on top if requested ####
  if (group_n == TRUE) {
    temp_row <- table_final[1, ]
    temp_row[] <- NA
    temp_row$parameter <- "n"
    
    for (loop_group in unique(input_data[[group_column]])) {
      temp_row[1, names(temp_row) == loop_group] <- 
        unique(table_stats$n[table_stats$group == loop_group])
    }
    table_final <- rbind(temp_row, table_final)
  }
  
  # 6. Return the silly thing ####
  
  # clear row names that were made weird by rbind and merge
  rownames(table_stats) <- NULL
  rownames(table_groupwise) <- NULL
  rownames(table_sex) <- NULL
  rownames(table_final) <- NULL
  
  # combine result tables for return
  cat(text_output)
  result <- list(
    report = text_output,
    demographic_statistics = table_stats,
    group_wise_comparisons = table_groupwise,
    sex_ratios = table_sex,
    complete_table = table_final
  )
  
  return(result)
}



