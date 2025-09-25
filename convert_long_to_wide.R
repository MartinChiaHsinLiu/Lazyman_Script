#' Convert Long Table to Wide Table
#'
#' This function efficiently converts a long-format data table to wide-format 
#' while preserving original data types and handling missing values appropriately.
#' It uses data.table's dcast function for optimal performance with large datasets.
#'
#' @param data A data.frame or data.table containing the long-format data to be converted.
#'   Must have columns specified in id_cols, feature_col, and value_col.
#' @param id_cols A character vector specifying the column names to use as identifier 
#'   variables. Default is c("s_id", "tp_id").
#' @param feature_col A character string specifying the column name that contains 
#'   the feature names (will become column headers). Default is "feature".
#' @param value_col A character string specifying the column name that contains 
#'   the values to be spread. Default is "value".
#' @param preserve_types A logical value indicating whether to preserve original data types.
#'   If TRUE (default), attempts to maintain numeric, logical, and character types.
#'   If FALSE, all values remain as their current type.
#' @param fill_na A logical value indicating whether to fill empty entries with 
#'   appropriate NA values based on data type. Default is TRUE.
#'
#' @return A tibble (data.frame) in wide format with:
#' \itemize{
#'   \item ID columns as specified in \code{id_cols}
#'   \item Feature columns with names from the \code{feature_col} values
#'   \item Values maintaining their original data types (if preserve_types = TRUE)
#' }
#'
#' @details
#' The function handles different data types as follows:
#' \itemize{
#'   \item \strong{Numeric}: Missing values filled with \code{NA_real_}
#'   \item \strong{Logical}: Missing values filled with \code{NA} (logical NA)
#'   \item \strong{Character}: Missing values filled with \code{NA_character_}
#'   \item \strong{Mixed types}: Attempts to preserve the most appropriate type
#' }
#'
#' When \code{preserve_types = TRUE}, the function:
#' \enumerate{
#'   \item Groups features by their detected data type
#'   \item Processes each type group separately using dcast
#'   \item Merges the results while maintaining type integrity
#'   \item Handles type conflicts by prioritizing the most restrictive type
#' }
#'
#' Empty or missing values are handled based on their context:
#' \itemize{
#'   \item Empty strings ("") are converted to NA
#'   \item "NULL" strings are converted to NA
#'   \item Existing NA values are preserved with appropriate type
#' }
#'
#' @section Performance:
#' This function is optimized for large datasets using data.table's dcast function. 
#' Performance scales well with the number of features and observations.
#'
#' @section Dependencies:
#' Requires the following packages: tidyr, dplyr, data.table
#'
#' @examples
#' # Create sample long data
#' long_data <- data.frame(
#'   s_id = rep(c("SAMPLE_001", "SAMPLE_002"), each = 4),
#'   tp_id = rep("TP_001", 8),
#'   feature = rep(c("age", "gender", "status", "score"), 2),
#'   value = c(25, "male", TRUE, 85.5, 30, "female", FALSE, 92.3),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Basic conversion with default settings
#' wide_data1 <- convert_long_to_wide(long_data)
#'
#' # Convert with custom column names
#' wide_data2 <- convert_long_to_wide(long_data,
#'                                   id_cols = "s_id",
#'                                   feature_col = "feature",
#'                                   value_col = "value")
#'
#' # Conversion without type preservation
#' wide_data3 <- convert_long_to_wide(long_data,
#'                                   preserve_types = FALSE)
#'
#' # Don't fill missing values with NA
#' wide_data4 <- convert_long_to_wide(long_data,
#'                                   fill_na = FALSE)
#'
#' @author Martin, Chia-Hsin Liu
#' @seealso \code{\link[tidyr]{pivot_wider}}, \code{\link[data.table]{dcast}}
#' @export
convert_long_to_wide <- function(data, id_cols = c("s_id", "tp_id"), 
                                feature_col = "feature", value_col = "value",
                                preserve_types = TRUE, fill_na = TRUE) {
    
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
    
    # 檢查必要的欄位是否存在
    required_cols <- c(id_cols, feature_col, value_col)
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop(paste("The following required columns do not exist in the data:", 
                  paste(missing_cols, collapse = ", ")))
    }
    
    cat("Converting long table to wide format...\n")
    cat("ID columns:", paste(id_cols, collapse = ", "), "\n")
    cat("Feature column:", feature_col, "\n")
    cat("Value column:", value_col, "\n")
    
    # 轉換為 data.table
    dt <- data.table::as.data.table(data)
    
    # 預處理空值
    if (fill_na) {
        cat("Preprocessing empty values...\n")
        dt[get(value_col) == "" | get(value_col) == "NULL", 
           (value_col) := NA]
    }
    
    if (!preserve_types) {
        # 快速模式：直接使用 dcast
        cat("Performing fast conversion (not preserving types)...\n")
        
        wide_dt <- data.table::dcast(
            dt,
            formula = paste(paste(id_cols, collapse = " + "), "~", feature_col),
            value.var = value_col,
            fill = if (fill_na) NA else NULL
        )
        
    } else {
        # 類型保留模式
        cat("Performing type-preserving conversion...\n")
        
        # 分析每個 feature 的數據類型
        feature_types <- dt[, {
            vals <- get(value_col)
            vals <- vals[!is.na(vals)]  # 排除 NA 來判斷類型
            
            if (length(vals) == 0) {
                "character"  # 預設為字符型
            } else {
                # 嘗試判斷主要類型
                if (all(vals %in% c("TRUE", "FALSE", TRUE, FALSE))) {
                    "logical"
                } else if (all(suppressWarnings(!is.na(as.numeric(as.character(vals)))))) {
                    "numeric"
                } else {
                    "character"
                }
            }
        }, by = get(feature_col)]
        
        setnames(feature_types, c("feature_name", "detected_type"))
        
        cat("Detected types:\n")
        type_summary <- feature_types[, .N, by = detected_type]
        for (i in seq_len(nrow(type_summary))) {
            cat(" ", type_summary$detected_type[i], ":", type_summary$N[i], "features\n")
        }
        
        # 分別處理不同類型的 features
        type_groups <- unique(feature_types$detected_type)
        wide_parts <- list()
        
        for (type_group in type_groups) {
            features_in_group <- feature_types[detected_type == type_group, feature_name]
            cat("Processing", length(features_in_group), type_group, "features...\n")
            
            # 篩選該類型的數據
            dt_subset <- dt[get(feature_col) %in% features_in_group]
            
            # 根據類型預處理值
            if (type_group == "numeric") {
                dt_subset[, (value_col) := as.numeric(as.character(get(value_col)))]
                fill_value <- if (fill_na) NA_real_ else NULL
            } else if (type_group == "logical") {
                dt_subset[, (value_col) := as.logical(as.character(get(value_col)))]
                fill_value <- if (fill_na) NA else NULL
            } else {  # character
                dt_subset[, (value_col) := as.character(get(value_col))]
                fill_value <- if (fill_na) NA_character_ else NULL
            }
            
            # 使用 dcast 轉換
            wide_part <- data.table::dcast(
                dt_subset,
                formula = paste(paste(id_cols, collapse = " + "), "~", feature_col),
                value.var = value_col,
                fill = fill_value
            )
            
            wide_parts[[type_group]] <- wide_part
        }
        
        # 合併所有部分
        cat("Merging results...\n")
        if (length(wide_parts) > 1) {
            wide_dt <- wide_parts[[1]]
            for (i in seq_along(wide_parts)[-1]) {
                wide_dt <- merge(wide_dt, wide_parts[[i]], 
                               by = id_cols, all = TRUE)
            }
        } else {
            wide_dt <- wide_parts[[1]]
        }
    }
    
    # 轉回 tibble 並整理
    cat("Finalizing results...\n")
    result <- wide_dt %>%
        as_tibble() %>%
        arrange(across(all_of(id_cols)))
    
    cat("Complete! Generated wide table with", ncol(result), "columns\n")
    cat("ID columns:", length(id_cols), "| Feature columns:", ncol(result) - length(id_cols), "\n")
    
    return(result)
}