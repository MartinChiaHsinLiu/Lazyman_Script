library(testthat)

test_that("convert_wide_to_long works correctly", {
  # Create sample wide data
  df <- data.frame(
    s_id = c(1, 2),
    tp_id = c("A", "B"),
    age = c(25, 30),
    name = c("John", "Jane"),
    active = c(TRUE, FALSE)
  )

  # Convert to long
  long_df <- convert_wide_to_long(df)

  # Check columns
  expect_true(all(c("s_id", "tp_id", "feature", "value_type", "value_num", "value_char", "value_logic") %in% names(long_df)))

  # Check rows
  expect_equal(nrow(long_df), 6)

  # Check value assignments
  age_rows <- long_df[long_df$feature == "age", ]
  expect_equal(age_rows$value_type[1], "numeric")
  expect_equal(age_rows$value_num, c(25, 30))

  name_rows <- long_df[long_df$feature == "name", ]
  expect_equal(name_rows$value_type[1], "character")
  expect_equal(name_rows$value_char, c("John", "Jane"))
})

test_that("convert_long_to_wide restores types correctly", {
  # Create sample wide data
  df <- data.frame(
    s_id = c(1, 2),
    tp_id = c("A", "B"),
    age = c(25, 30),
    name = c("John", "Jane"),
    active = c(TRUE, FALSE)
  )

  long_df <- convert_wide_to_long(df)
  restored_df <- convert_long_to_wide(long_df)
  
  # dcast sorts columns alphabetically, so we restore the original order for the identity check
  restored_df <- restored_df[, names(df)]

  # Check identity using the built-in function
  check_res <- check_data_identity(df, restored_df, verbose = FALSE)
  expect_true(check_res$identical)
})
