#' Convert Wide Table to Long Table with Type-Specific Columns
#'
#' @description
#' Converts a wide format data table to long format while preserving data types
#' in separate columns (value_num, value_char, value_logic). Each row will have
#' a value in exactly one of these columns based on the original data type.
#'
#' @param data A data frame or data table in wide format to be converted
#' @param id_cols Character vector of column names to use as ID variables.
#'   Default is c("s_id", "tp_id")
#' @param remove_empty Logical. If TRUE, removes rows where all value columns
#'   (value_num, value_char, value_logic) are NA. Default is FALSE
#'
#' @return A tibble in long format with columns:
#'   \itemize{
#'     \item ID columns (as specified in id_cols)
#'     \item feature: The original column name
#'     \item value_type: Data type ("numeric", "character", or "logical")
#'     \item value_num: Numeric values (NA for non-numeric types)
#'     \item value_char: Character values (NA for non-character types)
#'     \item value_logic: Logical values (NA for non-logical types)
#'   }
#'
#' @details
#' The function identifies the data type of each column in the wide format and
#' stores values in type-specific columns in the long format. The value_type
#' column is preserved to facilitate conversion back to wide format.
#'
#' Factor columns are treated as character type.
#'
#' @examples
#' \dontrun{
#' # Create sample wide data
#' wide_data <- data.frame(
#'   s_id = c(1, 2),
#'   tp_id = c("A", "B"),
#'   age = c(25, 30),
#'   name = c("John", "Jane"),
#'   active = c(TRUE, FALSE)
#' )
#'
#' # Convert to long format
#' long_data <- convert_wide_to_long(wide_data)
#'
#' # Convert with custom ID columns
#' long_data <- convert_wide_to_long(wide_data, id_cols = c("s_id"))
#'
#' # Remove empty values
#' long_data <- convert_wide_to_long(wide_data, remove_empty = TRUE)
#' }
#'
#' @export
#' @import data.table
#' @import dplyr
#' @import tidyr
convert_wide_to_long <- function(data, id_cols = c("s_id", "tp_id"), 
                                 remove_empty = FALSE) {
  check_packages()
  validate_id_cols(data, id_cols)
  
  all_cols <- names(data)
  value_cols <- setdiff(all_cols, id_cols)
  
  cat("Processing", length(value_cols), "columns...\n")
  
  dt <- data.table::as.data.table(data)
  type_map <- create_type_map(data, value_cols)
  
  print_type_summary(type_map)
  
  dt_for_melt <- prepare_data_for_melt(dt, value_cols)
  long_dt <- melt_data(dt_for_melt, id_cols)
  long_dt <- add_type_columns(long_dt, type_map)
  
  if (remove_empty) {
    long_dt <- remove_empty_values(long_dt)
  }
  
  result <- finalize_result(long_dt, id_cols)
  
  cat("Complete! Final dataset has", nrow(result), "rows\n")
  
  return(result)
}

check_packages <- function() {
  required_pkgs <- c("tidyr", "dplyr", "data.table")
  for (pkg in required_pkgs) {
    if (!require(pkg, quietly = TRUE, character.only = TRUE)) {
      stop(paste(pkg, "package is required"))
    }
  }
}

validate_id_cols <- function(data, id_cols) {
  missing_cols <- setdiff(id_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("The following specified ID columns do not exist:", 
               paste(missing_cols, collapse = ", ")))
  }
}

create_type_map <- function(data, value_cols) {
  type_map <- data.table::data.table()
  
  for (col in value_cols) {
    col_type <- determine_column_type(data[[col]])
    type_map <- rbind(type_map, 
                      data.table::data.table(feature = col, value_type = col_type))
  }
  
  return(type_map)
}

determine_column_type <- function(column) {
  if (is.factor(column)) {
    return("character")
  } else if (is.logical(column)) {
    return("logical")
  } else if (is.numeric(column)) {
    return("numeric")
  } else {
    return("character")
  }
}

print_type_summary <- function(type_map) {
  cat("Detected types:\n")
  type_summary <- type_map[, .N, by = value_type]
  for (i in seq_len(nrow(type_summary))) {
    cat("  ", type_summary$value_type[i], ":", type_summary$N[i], "features\n")
  }
}

prepare_data_for_melt <- function(dt, value_cols) {
  dt_for_melt <- data.table::copy(dt)
  for (col in value_cols) {
    data.table::set(dt_for_melt, j = col, value = as.character(dt_for_melt[[col]]))
  }
  return(dt_for_melt)
}

melt_data <- function(dt, id_cols) {
  long_dt <- data.table::melt(
    dt,
    id.vars = id_cols,
    variable.name = "feature",
    value.name = "value",
    variable.factor = FALSE
  )
  return(long_dt)
}

add_type_columns <- function(long_dt, type_map) {
  long_dt <- merge(long_dt, type_map, by = "feature", all.x = TRUE)
  
  cat("Converting values to type-specific columns...\n")
  
  long_dt[, value_num := ifelse(value_type == "numeric", 
                                suppressWarnings(as.numeric(value)), 
                                NA_real_)]
  long_dt[, value_char := ifelse(value_type == "character", 
                                 as.character(value), 
                                 NA_character_)]
  long_dt[, value_logic := ifelse(value_type == "logical", 
                                  as.logical(value), 
                                  NA)]
  
  long_dt[, value := NULL]
  
  return(long_dt)
}

remove_empty_values <- function(long_dt) {
  cat("Removing rows with no information...\n")
  original_rows <- nrow(long_dt)
  
  long_dt <- long_dt[!(is.na(value_num) & is.na(value_char) & is.na(value_logic))]
  
  removed_rows <- original_rows - nrow(long_dt)
  cat("Removed", removed_rows, "rows with no information\n")
  
  return(long_dt)
}

finalize_result <- function(long_dt, id_cols) {
  result <- long_dt %>% 
    dplyr::as_tibble() %>% 
    dplyr::arrange(dplyr::across(dplyr::all_of(id_cols)), feature)
  
  cat("Finalizing results...\n")
  
  return(result)
}
