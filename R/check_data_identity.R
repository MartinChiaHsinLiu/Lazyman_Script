#' Check Identity Between Original and Restored Data
#'
#' @description
#' Validates whether data restored from long format is identical to the original
#' wide format data. This function checks dimensions, column names, column types,
#' and values to ensure the conversion process preserved all information.
#'
#' @param original A data frame or data table representing the original wide format data
#' @param restored A data frame or data table representing the restored wide format data
#'   (after converting to long and back to wide)
#' @param id_cols Character vector of ID column names used in the conversion.
#'   Default is c("s_id", "tp_id")
#' @param tolerance Numeric tolerance for comparing numeric values. Default is 1e-8
#' @param verbose Logical. If TRUE, prints detailed comparison results. Default is TRUE
#'
#' @return A list with the following elements:
#'   \itemize{
#'     \item identical: Logical indicating if data are identical
#'     \item issues: Character vector describing any differences found
#'     \item summary: List containing detailed comparison results
#'   }
#'
#' @details
#' The function performs the following checks:
#' \itemize{
#'   \item Dimensions (number of rows and columns)
#'   \item Column names and order
#'   \item Column data types
#'   \item Values in each column (with tolerance for numeric comparisons)
#'   \item NA patterns
#' }
#'
#' For numeric columns, values are considered equal if they differ by less than
#' the specified tolerance.
#'
#' @examples
#' \dontrun{
#' # Original data
#' original_data <- data.frame(
#'   s_id = c(1, 2),
#'   tp_id = c("A", "B"),
#'   age = c(25, 30),
#'   name = c("John", "Jane")
#' )
#'
#' # Convert to long and back
#' long_data <- convert_wide_to_long(original_data)
#' restored_data <- convert_long_to_wide(long_data)
#'
#' # Check identity
#' result <- check_data_identity(original_data, restored_data)
#'
#' # Check with custom tolerance
#' result <- check_data_identity(original_data, restored_data, tolerance = 1e-6)
#'
#' # Silent check
#' result <- check_data_identity(original_data, restored_data, verbose = FALSE)
#' }
#'
#' @export
#' @import dplyr
check_data_identity <- function(original, restored, 
                                id_cols = c("s_id", "tp_id"),
                                tolerance = 1e-8,
                                verbose = TRUE) {
  
  issues <- character(0)
  summary_list <- list()
  
  if (verbose) cat("Checking data identity...\n\n")
  
  dim_check <- check_dimensions(original, restored, verbose)
  issues <- c(issues, dim_check$issues)
  summary_list$dimensions <- dim_check$result
  
  col_check <- check_column_names(original, restored, verbose)
  issues <- c(issues, col_check$issues)
  summary_list$columns <- col_check$result
  
  type_check <- check_column_types(original, restored, verbose)
  issues <- c(issues, type_check$issues)
  summary_list$types <- type_check$result
  
  value_check <- check_column_values(original, restored, tolerance, verbose)
  issues <- c(issues, value_check$issues)
  summary_list$values <- value_check$result
  
  is_identical <- length(issues) == 0
  
  print_final_result(is_identical, issues, verbose)
  
  return(list(
    identical = is_identical,
    issues = issues,
    summary = summary_list
  ))
}

check_dimensions <- function(original, restored, verbose) {
  issues <- character(0)
  
  orig_dim <- dim(original)
  rest_dim <- dim(restored)
  
  if (verbose) {
    cat("Dimension Check:\n")
    cat("  Original: ", orig_dim[1], "rows x", orig_dim[2], "columns\n")
    cat("  Restored: ", rest_dim[1], "rows x", rest_dim[2], "columns\n")
  }
  
  if (!identical(orig_dim, rest_dim)) {
    issues <- c(issues, sprintf(
      "Dimensions differ: original (%d x %d) vs restored (%d x %d)",
      orig_dim[1], orig_dim[2], rest_dim[1], rest_dim[2]
    ))
  }
  
  if (verbose) {
    cat("  Status:", if (length(issues) == 0) "PASS" else "FAIL", "\n\n")
  }
  
  return(list(issues = issues, result = list(original = orig_dim, restored = rest_dim)))
}

check_column_names <- function(original, restored, verbose) {
  issues <- character(0)
  
  orig_cols <- names(original)
  rest_cols <- names(restored)
  
  if (verbose) cat("Column Names Check:\n")
  
  missing_in_restored <- setdiff(orig_cols, rest_cols)
  extra_in_restored <- setdiff(rest_cols, orig_cols)
  
  if (length(missing_in_restored) > 0) {
    issues <- c(issues, sprintf(
      "Missing columns in restored: %s",
      paste(missing_in_restored, collapse = ", ")
    ))
    if (verbose) {
      cat("  Missing in restored:", paste(missing_in_restored, collapse = ", "), "\n")
    }
  }
  
  if (length(extra_in_restored) > 0) {
    issues <- c(issues, sprintf(
      "Extra columns in restored: %s",
      paste(extra_in_restored, collapse = ", ")
    ))
    if (verbose) {
      cat("  Extra in restored:", paste(extra_in_restored, collapse = ", "), "\n")
    }
  }
  
  if (!identical(orig_cols, rest_cols)) {
    issues <- c(issues, "Column order differs")
    if (verbose) cat("  Column order differs\n")
  }
  
  if (verbose) {
    cat("  Status:", if (length(issues) == 0) "PASS" else "FAIL", "\n\n")
  }
  
  return(list(
    issues = issues,
    result = list(
      original = orig_cols,
      restored = rest_cols,
      missing = missing_in_restored,
      extra = extra_in_restored
    )
  ))
}

check_column_types <- function(original, restored, verbose) {
  issues <- character(0)
  type_diffs <- list()
  
  if (verbose) cat("Column Types Check:\n")
  
  common_cols <- intersect(names(original), names(restored))
  
  for (col in common_cols) {
    orig_type <- class(original[[col]])[1]
    rest_type <- class(restored[[col]])[1]
    
    if (orig_type != rest_type) {
      type_diffs[[col]] <- c(original = orig_type, restored = rest_type)
      issues <- c(issues, sprintf(
        "Type mismatch in '%s': original (%s) vs restored (%s)",
        col, orig_type, rest_type
      ))
      if (verbose) {
        cat("  ", col, ":", orig_type, "->", rest_type, "\n")
      }
    }
  }
  
  if (verbose) {
    cat("  Status:", if (length(issues) == 0) "PASS" else "FAIL", "\n\n")
  }
  
  return(list(issues = issues, result = type_diffs))
}

check_column_values <- function(original, restored, tolerance, verbose) {
  issues <- character(0)
  value_diffs <- list()
  
  if (verbose) cat("Column Values Check:\n")
  
  common_cols <- intersect(names(original), names(restored))
  
  for (col in common_cols) {
    col_issues <- compare_column_values(
      original[[col]], 
      restored[[col]], 
      col, 
      tolerance
    )
    
    if (length(col_issues) > 0) {
      issues <- c(issues, col_issues)
      value_diffs[[col]] <- col_issues
      if (verbose) cat("  ", col, ": FAIL\n")
    } else {
      if (verbose) cat("  ", col, ": PASS\n")
    }
  }
  
  if (verbose) cat("\n")
  
  return(list(issues = issues, result = value_diffs))
}

compare_column_values <- function(orig_col, rest_col, col_name, tolerance) {
  issues <- character(0)
  
  if (is.numeric(orig_col) && is.numeric(rest_col)) {
    max_diff <- max(abs(orig_col - rest_col), na.rm = TRUE)
    if (is.finite(max_diff) && max_diff > tolerance) {
      issues <- c(issues, sprintf(
        "Numeric values differ in '%s': max difference = %.2e",
        col_name, max_diff
      ))
    }
  } else {
    if (!identical(orig_col, rest_col)) {
      diff_count <- sum(orig_col != rest_col, na.rm = TRUE)
      issues <- c(issues, sprintf(
        "Values differ in '%s': %d differences found",
        col_name, diff_count
      ))
    }
  }
  
  na_orig <- sum(is.na(orig_col))
  na_rest <- sum(is.na(rest_col))
  if (na_orig != na_rest) {
    issues <- c(issues, sprintf(
      "NA count differs in '%s': original (%d) vs restored (%d)",
      col_name, na_orig, na_rest
    ))
  }
  
  return(issues)
}

print_final_result <- function(is_identical, issues, verbose) {
  if (!verbose) return(invisible(NULL))
  
  cat("=", rep("=", 60), "\n", sep = "")
  cat("FINAL RESULT:", if (is_identical) "IDENTICAL" else "NOT IDENTICAL", "\n")
  
  if (!is_identical) {
    cat("\nIssues found:\n")
    for (i in seq_len(length(issues))) {
      cat("  ", i, ". ", issues[i], "\n", sep = "")
    }
  }
  
  cat("=", rep("=", 60), "\n", sep = "")
}
