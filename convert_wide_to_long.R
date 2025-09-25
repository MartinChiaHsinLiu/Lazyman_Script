
#' Convert Wide Table to Long Table
#'
#' This function efficiently converts a wide-format data table to long-format 
#' while preserving original data types and handling various data type combinations.
#' It uses data.table's melt function for optimal performance with large datasets.
#'
#' @param data A data.frame or data.table containing the wide-format data to be converted.
#' @param id_cols A character vector specifying the column names to keep as identifier 
#'   variables. Default is c("s_id", "tp_id").
#' @param preserve_types A logical value indicating whether to preserve original data types.
#'   If TRUE (default), numeric, character, and logical columns maintain their types.
#'   If FALSE, all values are converted to character for faster processing.
#' @param remove_empty A logical value indicating whether to remove rows with no 
#'   information (NA, NULL, NaN, empty strings). Default is FALSE.
#'
#' @return A tibble (data.frame) in long format with the following columns:
#' \itemize{
#'   \item ID columns as specified in \code{id_cols}
#'   \item \code{feature}: Column names from the original wide table (character)
#'   \item \code{value}: Values from the original wide table (mixed types if preserve_types = TRUE)
#' }
#'
#' @details
#' The function handles different data types as follows:
#' \itemize{
#'   \item \strong{Factor}: Automatically converted to character
#'   \item \strong{Numeric}: Preserved as numeric (if preserve_types = TRUE)
#'   \item \strong{Logical}: Preserved as logical (if preserve_types = TRUE)  
#'   \item \strong{Character}: Preserved as character
#' }
#'
#' When \code{preserve_types = TRUE}, the function processes different data types 
#' separately to avoid type coercion, then combines the results using data.table's 
#' rbindlist for efficiency.
#'
#' When \code{remove_empty = TRUE}, the following values are removed:
#' \itemize{
#'   \item NA values
#'   \item NULL values
#'   \item NaN values (for numeric data)
#'   \item Empty strings ("")
#'   \item Values that become NA when converted to character
#' }
#'
#' @section Performance:
#' This function is optimized for large datasets (tested with 380+ columns). 
#' For maximum speed with very large datasets, set \code{preserve_types = FALSE}.
#'
#' @section Dependencies:
#' Requires the following packages: tidyr, dplyr, data.table
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   s_id = c("SAMPLE_001", "SAMPLE_002", "SAMPLE_003"),
#'   tp_id = c("TP_001", "TP_001", "TP_001"),
#'   age = c(25, 30, 35),
#'   gender = c("male", "female", "male"),
#'   status = c(TRUE, FALSE, TRUE),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Basic conversion with default settings
#' long_data1 <- convert_wide_to_long(sample_data)
#'
#' # Convert with custom ID columns
#' long_data2 <- convert_wide_to_long(sample_data, 
#'                                   id_cols = "s_id")
#'
#' # Fast conversion without type preservation
#' long_data3 <- convert_wide_to_long(sample_data, 
#'                                   preserve_types = FALSE)
#'
#' # Remove empty values
#' long_data4 <- convert_wide_to_long(sample_data, 
#'                                   remove_empty = TRUE)
#'
#' # Combine all options
#' long_data5 <- convert_wide_to_long(sample_data,
#'                                   id_cols = c("s_id", "tp_id"),
#'                                   preserve_types = TRUE,
#'                                   remove_empty = TRUE)
#'
#' @author Martin, Chia-Hsin Liu
#' @seealso \code{\link[tidyr]{pivot_longer}}, \code{\link[data.table]{melt}}
#' @export

convert_wide_to_long <- function(data, id_cols = c("s_id", "tp_id"), 
                                 preserve_types = TRUE, remove_empty = FALSE) {
  # 檢查必要的套件
  if (!require(tidyr, quietly = TRUE)) {
    stop("tidyr package is required")
  }
  if (!require(dplyr, quietly = TRUE)) {
    stop("dplyr package is required")
  }
  if (!require(data.table, quietly = TRUE)) {
    stop("data.table package is required")
  }
  
  # 檢查指定的 ID 欄位是否存在
  missing_cols <- setdiff(id_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("The following specified ID columns do not exist in the data:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  # 識別要轉換的欄位
  all_cols <- names(data)
  value_cols <- setdiff(all_cols, id_cols)
  
  cat("Processing", length(value_cols), "columns...\n")
  
  # 轉換為 data.table
  dt <- data.table::as.data.table(data)
  
  if (!preserve_types) {
    # 如果不需要保留類型，直接使用 melt（最快）
    cat("Performing fast conversion (all values will be converted to character)...\n")
    
    long_dt <- data.table::melt(
      dt, 
      id.vars = id_cols,
      variable.name = "feature",
      value.name = "value",
      variable.factor = FALSE
    )
    
  } else {
    # 保留類型：分別處理不同類型的欄位
    cat("Performing type-preserving conversion...\n")
    
    # 分類欄位
    numeric_cols <- character(0)
    character_cols <- character(0)
    logical_cols <- character(0)
    
    for (col in value_cols) {
      if (is.factor(dt[[col]])) {
        dt[, (col) := as.character(get(col))]
        character_cols <- c(character_cols, col)
      } else if (is.logical(dt[[col]])) {
        logical_cols <- c(logical_cols, col)
      } else if (is.numeric(dt[[col]])) {
        numeric_cols <- c(numeric_cols, col)
      } else {
        character_cols <- c(character_cols, col)
      }
    }
    
    cat("Numeric columns:", length(numeric_cols), "\n")
    cat("Character columns:", length(character_cols), "\n")  
    cat("Logical columns:", length(logical_cols), "\n")
    
    # 分別 melt 不同類型的欄位
    result_list <- list()
    
    if (length(numeric_cols) > 0) {
      numeric_dt <- data.table::melt(
        dt[, .SD, .SDcols = c(id_cols, numeric_cols)],
        id.vars = id_cols,
        variable.name = "feature", 
        value.name = "value",
        variable.factor = FALSE
      )
      result_list <- append(result_list, list(numeric_dt))
    }
    
    if (length(character_cols) > 0) {
      character_dt <- data.table::melt(
        dt[, .SD, .SDcols = c(id_cols, character_cols)],
        id.vars = id_cols,
        variable.name = "feature",
        value.name = "value", 
        variable.factor = FALSE
      )
      result_list <- append(result_list, list(character_dt))
    }
    
    if (length(logical_cols) > 0) {
      logical_dt <- data.table::melt(
        dt[, .SD, .SDcols = c(id_cols, logical_cols)],
        id.vars = id_cols,
        variable.name = "feature",
        value.name = "value",
        variable.factor = FALSE
      )
      result_list <- append(result_list, list(logical_dt))
    }
    
    # 合併結果
    if (length(result_list) > 1) {
      long_dt <- data.table::rbindlist(result_list)
    } else if (length(result_list) == 1) {
      long_dt <- result_list[[1]]
    } else {
      stop("No columns found to convert")
    }
  }
  
  # 轉回 tibble 格式並排序
  cat("Finalizing results...\n")
  result <- long_dt %>%
    as_tibble() %>%
    arrange(across(all_of(id_cols)), feature)
  
  # 根據參數決定是否移除沒有資訊的行
  if (remove_empty) {
    cat("Removing rows with no information (NA, NULL, NaN, empty string)...\n")
    original_rows <- nrow(result)
    
    result <- result %>%
      filter(
        !is.na(value) &                    # 移除 NA
          !is.null(value) &                  # 移除 NULL
          !(is.numeric(value) & is.nan(value)) &  # 移除 NaN（針對數值型）
          value != "" &                      # 移除空字串
          !is.na(as.character(value))        # 移除轉為字元後是 NA 的值
      )
    
    removed_rows <- original_rows - nrow(result)
    cat("Removed", removed_rows, "rows with no information\n")
    cat("Complete! Final dataset has", nrow(result), "rows\n")
  } else {
    cat("Complete! Generated", nrow(result), "rows (including empty values)\n")
  }
  
  return(result)
}

sample_data <- data.frame(
    s_id = c("ICGC-BLCA-DO48360", "ICGC-BLCA-DO48361", "ICGC-BLCA-DO48362"),
    tp_id = c("TP_034", "TP_034", "TP_034"),
    cancer_type_abbr = c("BLCA", "BLCA", "BLCA"),
    age_at_diagnosis = c(52, 25, 69),
    days_to_last_followup = c(NA, NA, NA),
    gender = c("male", "male", "female"),
    include_in_driverdbv4 = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
)
