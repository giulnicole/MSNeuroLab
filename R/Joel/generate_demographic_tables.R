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
#' @param format_value Number of decimal places for percentage formatting.
#' @param round_value Number of decimal places for rounding statistics.
#' @param completed_table_statistic Which statistic to show in the final table. 
#'        Options: "mean_sd", "median_Q1_Q3", "min_max", "mean", "sd", "median", "Q1", "Q3", "min", "max".
#' @param completed_table_sex_statistic Which sex statistic to show in the final table.
#'        Options: "n_m_f", "n_fn_ratio", "n_f_fn_ratio", "f_n_ratio", "n_f", "n_m", "n".
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
#' print(results$sex_ratios)
#' print(results$complete_table)
#' }                             
#'
#' @export
generate_demographic_tables <- function(
    input_data = mtcars,                           # Input data frame
    parameter_list = c("mpg", "disp", "qsec"),     # Columns to analyze
    group_column = "am",                           # Grouping variable
    sex_column = "vs",                            # Sex variable (optional)
    female_value = 0,                             # Female identifier (optional, but required for sex_column)
    male_value = 1,                               # Male identifier (optional, but required for sex_column)
    format_value = 2,                              # Decimal places for percentages
    round_value = 2,                               # Decimal places for rounding
    completed_table_statistic = "median_Q1_Q3",     # Statistic for final table
    completed_table_sex_statistic = "n_m_f",       # Sex statistic for final table
    multiple_comparison_correction = "BH",          # P-value correction method (NULL to skip)
    comparison_list = NULL                         # Specific comparisons (optional, will limit group wise comparisons to the ones in this list)
) {
  
  # Dependencies
  required_packages <- c("tibble", "tidyr")
  
  # Check for missing packages
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  if(length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages), "\n")
    install.packages(missing_packages, repos = "https://cran.r-project.org")
  }
  
  require(tibble)
  require(tidyr)
  
  # 1. Collect all possible group comparisons
  group_values <- unique(input_data[[group_column]])
  
  if (length(group_values) > 2) {
    all_comparisons <- t(combn(as.character(group_values), 2))
    all_comparisons <- split(all_comparisons, seq(nrow(all_comparisons)))
  } else {
    # Handle case with only 2 groups
    all_comparisons <- list(c(group_values[1], group_values[2]))
  }
  
  # Sort each comparison pair alphabetically
  all_comparisons <- lapply(all_comparisons, sort)
  
  # Use specific comparisons if provided
  if(!is.null(comparison_list)) {
    comparison_list <- lapply(comparison_list, sort)
    all_comparisons <- all_comparisons[all_comparisons %in% comparison_list]
  }
  
  # 2. Calculate descriptive statistics
  table_stats <- data.frame()
  
  for (loop_group in unique(input_data[[group_column]])) {
    for (loop_column in parameter_list) {
      collection <- input_data[input_data[[group_column]] == loop_group, ]
      
      # Create row for current parameter and group
      temp <- data.frame(
        parameter = loop_column,
        group = loop_group,
        n = nrow(collection),
        mean = round(mean(collection[[loop_column]], na.rm = TRUE), round_value),
        sd = round(sd(collection[[loop_column]], na.rm = TRUE), round_value),
        median = round(median(collection[[loop_column]], na.rm = TRUE), round_value),
        Q1 = round(quantile(collection[[loop_column]], na.rm = TRUE)[2], round_value),
        Q3 = round(quantile(collection[[loop_column]], na.rm = TRUE)[4], round_value),
        min = round(min(collection[[loop_column]], na.rm = TRUE), round_value),
        max = round(max(collection[[loop_column]], na.rm = TRUE), round_value),
        stringsAsFactors = FALSE
      )
      
      # Format combined statistics
      temp$mean_sd <- paste0(format(temp$mean, nsmall = round_value), 
                             " (", format(temp$sd, nsmall = round_value), ")")
      temp$median_Q1_Q3 <- paste0(format(temp$median, nsmall = round_value), 
                                  " (", format(temp$Q1, nsmall = round_value), 
                                  " - ", format(temp$Q3, nsmall = round_value), ")")
      temp$min_max <- paste0(format(temp$min, nsmall = round_value), 
                             " - ", format(temp$max, nsmall = round_value))
      
      # Shapiro-Wilk test for normality (if appropriate)
      if (sum(!is.na(collection[[loop_column]])) > 2 && 
          sum(!is.na(collection[[loop_column]])) < 5001) {
        temp$shapiro_p <- format(round(
          shapiro.test(collection[[loop_column]])$p.value, 3), nsmall = 3)
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
  
  # 3. Calculate sex statistics if sex column is provided
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
      temp$f_n_ratio <- format(round(temp$n_f/temp$n * 100, round_value), 
                               nsmall = format_value)
      temp$n_fn_ratio <- paste0(temp$n, " (", temp$f_n_ratio, ")")
      temp$n_f_fn_ratio <- paste0(temp$n_f, " (", temp$f_n_ratio, ")")
      
      table_sex <- rbind(table_sex, temp)
    }
    
    # Fisher's exact test for sex distribution
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
        as.numeric(fisher.test(temp_contingency)["p.value"]), 3), nsmall = 3)
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
  
  # 4. Wilcoxon test for each parameter and comparison
  wilcox_results <- data.frame()
  
  for (loop_parameter in parameter_list) {
    temp_results <- data.frame(parameter = loop_parameter, stringsAsFactors = FALSE)
    
    for (loop_comparison in 1:length(all_comparisons)) {
      variable1 = all_comparisons[[loop_comparison]][1]
      variable2 = all_comparisons[[loop_comparison]][2]
      
      # Extract data for the two groups
      data1 <- na.omit(input_data[input_data[[group_column]] == variable1, loop_parameter])
      data2 <- na.omit(input_data[input_data[[group_column]] == variable2, loop_parameter])
      
      # Perform Wilcoxon test if both groups have data
      if (length(data1) > 0 && length(data2) > 0) {
        wilcox_p <- wilcox.test(data1, data2)$p.value
        comparison_name <- paste0(variable1, " vs ", variable2)
        temp_results[1, comparison_name] <- format(round(wilcox_p, 3), nsmall = 3)
      } else {
        comparison_name <- paste0(variable1, " vs ", variable2)
        temp_results[1, comparison_name] <- NA
      }
    }
    
    wilcox_results <- rbind(wilcox_results, temp_results)
  }
  
  # 5. Create final formatted table
  wide_table <- pivot_wider(
    table_stats[c("parameter", "group", completed_table_statistic)],
    names_from = group,
    values_from = completed_table_statistic
  )
  
  # Merge with statistical test results
  table_final <- merge(wide_table, wilcox_results, by = "parameter", all.x = TRUE)
  
  # Add sex ratios if sex column is defined
  if (!is.null(sex_column)) {
    table_final <- rbind(
      table_sex[table_sex$parameter == completed_table_sex_statistic, ],
      table_final
    )
  }
  
  # Apply multiple comparison correction
  if(!is.null(multiple_comparison_correction)) {
    p_value_columns <- grep(" vs ", names(table_final))
    for (loop_row in 1:nrow(table_final)) {
      table_final[loop_row, p_value_columns] <- format(p.adjust(
        p = as.numeric(table_final[loop_row, p_value_columns]), 
        method = multiple_comparison_correction
      ), nsmall = 3)
    }
  }
  
  # Add group sizes as first row
  temp_row <- table_final[1, ]
  temp_row[] <- NA
  temp_row$parameter <- "n"
  
  for (loop_group in unique(input_data[[group_column]])) {
    temp_row[1, names(temp_row) == loop_group] <- 
      unique(table_stats$n[table_stats$group == loop_group])
  }
  
  table_final <- rbind(temp_row, table_final)
  
  # Add column names as first row
  table_final <- rbind(colnames(table_final), table_final)
  
  # 6. Return results
  result <- list(
    demographic_statistics = table_stats,
    sex_ratios = table_sex,
    complete_table = table_final
  )
  
  return(result)
}