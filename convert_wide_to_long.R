
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
    # 保留類型：但由於 R 的限制，同一個 value 欄位無法容納不同類型
    # 我們將所有值轉為字符型，但添加一個 value_type 欄位來記錄原始類型
    cat("Performing type-preserving conversion with type annotation...\n")
    
    # 添加類型信息欄位
    dt_with_type <- copy(dt)
    
    # 為每個 feature 添加其數據類型信息
    for (col in value_cols) {
      if (is.factor(dt_with_type[[col]])) {
        dt_with_type[, paste0(col, "_type") := "character"]
        dt_with_type[, (col) := as.character(get(col))]
      } else if (is.logical(dt_with_type[[col]])) {
        dt_with_type[, paste0(col, "_type") := "logical"]
      } else if (is.numeric(dt_with_type[[col]])) {
        dt_with_type[, paste0(col, "_type") := "numeric"]  
      } else {
        dt_with_type[, paste0(col, "_type") := "character"]
      }
    }
    
    # 創建類型映射
    type_map <- data.table()
    for (col in value_cols) {
      col_type <- if (is.factor(data[[col]])) {
        "character"
      } else if (is.logical(data[[col]])) {
        "logical"
      } else if (is.numeric(data[[col]])) {
        "numeric"
      } else {
        "character"
      }
      
      type_map <- rbind(type_map, 
                        data.table(feature = col, value_type = col_type))
    }
    
    cat("Detected types:\n")
    type_summary <- type_map[, .N, by = value_type]
    for (i in seq_len(nrow(type_summary))) {
      cat(" ", type_summary$value_type[i], ":", type_summary$N[i], "features\n")
    }
    
    # 使用 melt，但先將所有值轉為字符型以避免衝突
    dt_for_melt <- copy(dt)
    for (col in value_cols) {
      dt_for_melt[, (col) := as.character(get(col))]
    }
    
    long_dt <- data.table::melt(
      dt_for_melt,
      id.vars = id_cols,
      variable.name = "feature",
      value.name = "value",
      variable.factor = FALSE
    )
    
    # 添加類型信息
    long_dt <- merge(long_dt, type_map, by = "feature", all.x = TRUE)
    
    # 根據參數決定是否移除沒有資訊的行
    if (remove_empty) {
      cat("Removing rows with no information (NA, NULL, NaN, empty string)...\n")
      original_rows <- nrow(long_dt)
      
      long_dt <- long_dt %>%
        filter(
          !is.na(value) &                    # 移除 NA
            !is.null(value) &                  # 移除 NULL
            !(is.numeric(value) & is.nan(value)) &  # 移除 NaN（針對數值型）
            value != "" &                      # 移除空字串
            !is.na(as.character(value))        # 移除轉為字元後是 NA 的值
        )
      
      removed_rows <- original_rows - nrow(long_dt)
      cat("Removed", removed_rows, "rows with no information\n")
    }
    
    # 根據類型轉換 value
    cat("Converting values back to original types...\n")
    #long_dt[value_type == "numeric" & !is.na(value) & value != "", value := as.character(as.numeric(value))]
    #long_dt[value_type == "logical" & !is.na(value) & value != "", value := as.character(as.logical(value))]
    type_converter <- function(val, type) {
      switch(type,
             numeric = as.numeric(val),
             logical = as.logical(val),
             character = as.character(val),
             as.character(val) # 若無對應型態，則回傳字串
      )
    }
    
    # 使用 mapply 進行轉換並新增為新的 list-column 'new_value'
    long_dt[, new_value := mapply(type_converter, value, value_type, SIMPLIFY = FALSE)]
    long_dt <- long_dt %>% dplyr::select(-value) %>% dplyr::rename(value=new_value)
    
    # 移除類型欄位（保持簡潔）
    #long_dt[, value_type := NULL]
  }
  
  # 轉回 tibble 格式並排序
  cat("Finalizing results...\n")
  result <- long_dt %>% as_tibble() %>% arrange(across(all_of(id_cols)), feature)
  
  cat("Complete! Final dataset has", nrow(result), "rows\n")
  cat("Complete! Generated", nrow(result), "rows (including empty values)\n")
  
  return(result)
}



sample_data <- data.frame(
    s_id = c("ICGC-BLCA-DO48360", "ICGC-BLCA-DO48361", "ICGC-BLCA-DO48362"),
    tp_id = c("TP_034", "TP_034", "TP_034"),
    cancer_type_abbr = c("BLCA", "BLCA", "BLCA"),
    age_at_diagnosis = c(52, 25, 69),
    days_to_last_followup = c(10, 20, 30),
    gender = c("male", "male", "female"),
    include_in_study = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
)
