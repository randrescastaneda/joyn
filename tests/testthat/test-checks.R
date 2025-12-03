# Load helper data
data <- make_test_data()

# Extract datasets
x1 <- data$x1
x1_duplicates <- data$x1_duplicates
x2 <- data$x2
x3 <- data$x3
y1 <- data$y1
y2 <- data$y2
y3 <- data$y3
df1 <- data$df1
df2 <- data$df2

# ----------------- TESTS -----------------

test_that("check_xy works as expected", {
  # Zero-column dataframe (invalid - should error)
  zero_col_df <- data.frame()

  # Zero-row dataframe with columns (valid - should warn)
  zero_row_df <- x1[0, ]

  # Errors - missing arguments
  expect_error(
    check_xy(x = x1)
  )
  expect_error(
    check_xy(y = y1)
  )

  # Zero-column inputs should error
  clear_joynenv()
  expect_error(
    check_xy(x = zero_col_df, y = y1),
    "wrong input specification"
  )

  clear_joynenv()
  expect_error(
    check_xy(x = x1, y = zero_col_df),
    "wrong input specification"
  )

  clear_joynenv()
  expect_error(
    check_xy(x = zero_col_df, y = zero_col_df),
    "wrong input specification"
  )

  # Zero-row inputs now generate warnings, not errors
  clear_joynenv()
  expect_no_error(
    check_xy(x = zero_row_df, y = y1)
  )
  expect_true(rlang::env_has(.joynenv, "joyn_msgs"))
  expect_equal(rlang::env_get(.joynenv, "joyn_msgs")$type, "warn")

  clear_joynenv()
  expect_no_error(
    check_xy(x = x1, y = zero_row_df)
  )
  expect_true(rlang::env_has(.joynenv, "joyn_msgs"))
  expect_equal(rlang::env_get(.joynenv, "joyn_msgs")$type, "warn")

  clear_joynenv()
  expect_no_error(
    check_xy(x = zero_row_df, y = zero_row_df)
  )
  expect_true(rlang::env_has(.joynenv, "joyn_msgs"))
  expect_equal(rlang::env_get(.joynenv, "joyn_msgs")$type, "warn")

  # No msg when no duplicate names
  clear_joynenv()
  expect_no_error(
    check_xy(x = x1, y = y1)
  )

  # Duplicate names still cause errors
  clear_joynenv()
  expect_error(
    check_xy(x = x1_duplicates, y = y1)
  )
})

test_that("check_duplicate_names works as expected", {
  expect_equal(
    check_duplicate_names(x1_duplicates, "x"),
    TRUE
  )
  expect_equal(
    check_duplicate_names(x1, "x"),
    FALSE
  )

  clear_joynenv()
  check_duplicate_names(
    x1_duplicates,
    "x"
  )
  expect_true(
    rlang::env_has(.joynenv, "joyn_msgs")
  )
})

test_that("check_reportvar works as expected", {
  expect_no_error(
    check_reportvar(reportvar = NULL)
  )
  expect_no_error(
    check_reportvar(reportvar = FALSE)
  )
  expect_error(
    check_reportvar(reportvar = 2)
  )
  expect_equal(
    check_reportvar(reportvar = NULL),
    check_reportvar(reportvar = FALSE)
  )
  expect_equal(
    check_reportvar(reportvar = FALSE),
    NULL
  )
  expect_equal(
    check_reportvar(reportvar = ".joyn"),
    ".joyn"
  )
})

test_that("check_by_vars works as expected", {
  res <- check_by_vars(
    by = "id",
    x = x1,
    y = y1
  )

  expect_equal(
    class(res),
    "list"
  )
  expect_equal(
    names(res),
    c("by", "xby", "yby", "tempkey")
  )
  expect_equal(
    res$by,
    "id"
  )
  expect_error(
    check_by_vars(by = NULL, x = x1, y = y1)
  )
})

test_that("check_match_type works as expected", {
  expect_error(
    check_match_type(x1, y1, by = NULL)
  )

  expect_error(
    check_match_type(x1, y1, by = "id", match_type = "invalid match_type")
  )

  clear_joynenv()
  expect_error(
    check_match_type(x1, y1, by = "id", match_type = "1:1")
  )
  expect_equal(
    rlang::env_get(.joynenv, "joyn_msgs")$type,
    "err"
  )

  clear_joynenv()
  expect_error(
    check_match_type(x3, y3, by = "id", match_type = "1:1")
  )
  expect_equal(
    rlang::env_get(.joynenv, "joyn_msgs")$type,
    "err"
  )

  clear_joynenv()
  expect_error(
    check_match_type(df1, df2, by = "id1", match_type = "1:1")
  )
  expect_equal(
    rlang::env_get(.joynenv, "joyn_msgs")$type,
    c("err", "err")
  )

  clear_joynenv()
  expect_error(
    check_match_type(x2, y3, by = "id", match_type = "m:1")
  )
  expect_equal(
    rlang::env_get(.joynenv, "joyn_msgs")$type,
    c("err")
  )

  clear_joynenv()
  check_match_type(
    x = x3,
    y = y3,
    by = "id",
    match_type = "m:m"
  )
  expect_contains(
    rlang::env_get(.joynenv, "joyn_msgs")$type,
    "warn"
  )

  clear_joynenv()
  expect_equal(
    check_match_type(x3, y2, by = "id", match_type = "1:1"),
    c("1", "1")
  )
  expect_equal(
    check_match_type(df1, df2, by = "id1", match_type = "m:m"),
    c("m", "m")
  )
})

test_that("y_vars_to_keep works", {
  by <- "id"

  expect_error(
    check_y_vars_to_keep("f", y1, by)
  )
  expect_error(
    check_y_vars_to_keep(c("F", "r"), y1, by)
  )
  expect_error(
    check_y_vars_to_keep(c(TRUE, TRUE), y1, by)
  )
  expect_error(
    check_y_vars_to_keep(NA, y1, by)
  )

  expect_null(
    check_y_vars_to_keep(NULL, y1, by)
  )
  expect_null(
    check_y_vars_to_keep(FALSE, y1, by)
  )
  expect_equal(
    check_y_vars_to_keep("y", y1, by),
    "y"
  )
  expect_equal(
    check_y_vars_to_keep(c("id", "y"), y1, by),
    "y"
  )
  expect_equal(
    check_y_vars_to_keep(TRUE, y1, by),
    "y"
  )
})

test_that("check_new_y_vars works", {
  expect_equal(
    check_new_y_vars(
      x = df1,
      by = "id1",
      y_vars_to_keep = "salary"
    ),
    "salary"
  )

  expect_equal(
    check_new_y_vars(
      x = df1,
      by = "id1",
      y_vars_to_keep = c("id2", "salary")
    ),
    c("id2.y", "salary")
  )

  expect_equal(
    class(
      check_new_y_vars(
        x = df1,
        by = "id1",
        y_vars_to_keep = "salary"
      )
    ),
    "character"
  )
})

test_that("is_valid_m_key works", {
  expect_error(
    is_valid_m_key(x1, by = 2)
  )
  expect_equal(
    is_valid_m_key(x1, by = "id"),
    TRUE
  )
  expect_equal(
    is_valid_m_key(x1, by = c("id", "x")),
    FALSE
  )
})

test_that("check_var_class works with inheritance", {
  clear_joynenv()
  dt <- data.table(
    a = 1:3, # integer
    b = letters[1:3], # character
    d = as.IDate(Sys.Date()) # inherits from "Date"
  )

  # All valid classes
  expect_null(check_var_class(dt, c("a", "b", "d")))

  # Add a list column (unsupported)
  dt$c <- list(1, 2, 3)
  clear_joynenv()
  res <- check_var_class(dt, "c")

  # Function should return invisible("c")
  expect_identical(res, "c")

  # Environment should have stored a message
  expect_true(rlang::env_has(.joynenv, "joyn_msgs"))
  msg <- rlang::env_get(.joynenv, "joyn_msgs")

  # Warning should mention the variable and class
  expect_true(grepl("class", msg$msg))
  expect_true(grepl("c", msg$msg))

  # Add multiple bad vars
  dt$e <- as.list(1:3)
  clear_joynenv()
  res_multi <- check_var_class(dt, c("a", "c", "e"))
  expect_identical(sort(res_multi), sort(c("c", "e")))
})

test_that("check_var_class handles NULL and empty inputs", {
  dt <- data.table(a = 1:3, b = letters[1:3])

  # NULL input
  expect_null(check_var_class(dt, NULL))

  # Empty character vector
  expect_null(check_var_class(dt, character(0)))

  # Zero-length input
  expect_null(check_var_class(dt, character()))
})

test_that("check_var_class errors on missing variables", {
  dt <- data.table(a = 1:3, b = letters[1:3])

  # Single missing variable
  expect_error(
    check_var_class(dt, "nonexistent"),
    "not found in the table"
  )

  # Multiple missing variables
  expect_error(
    check_var_class(dt, c("x", "y", "z")),
    "not found in the table"
  )

  # Mix of existing and missing
  expect_error(
    check_var_class(dt, c("a", "missing")),
    "not found in the table"
  )
})

test_that("check_var_class handles all allowed classes", {
  dt <- data.table(
    char_col = letters[1:3],
    int_col = 1:3,
    num_col = c(1.1, 2.2, 3.3),
    fac_col = factor(c("a", "b", "c")),
    log_col = c(TRUE, FALSE, TRUE),
    date_col = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    posix_col = as.POSIXct(c(
      "2024-01-01 12:00:00",
      "2024-01-02 12:00:00",
      "2024-01-03 12:00:00"
    ))
  )

  # All allowed classes should return NULL
  clear_joynenv()
  expect_null(check_var_class(dt, names(dt)))
})

test_that("check_var_class handles unsupported classes", {
  dt <- data.table(
    a = 1:3,
    list_col = list(1, 2, 3),
    complex_col = complex(real = 1:3, imaginary = 1:3),
    raw_col = as.raw(1:3)
  )

  # List column
  clear_joynenv()
  res <- check_var_class(dt, "list_col")
  expect_identical(res, invisible("list_col"))
  expect_true(rlang::env_has(.joynenv, "joyn_msgs"))

  # Complex column
  clear_joynenv()
  res <- check_var_class(dt, "complex_col")
  expect_identical(res, invisible("complex_col"))

  # Raw column
  clear_joynenv()
  res <- check_var_class(dt, "raw_col")
  expect_identical(res, invisible("raw_col"))

  # Multiple unsupported
  clear_joynenv()
  res <- check_var_class(dt, c("list_col", "complex_col", "raw_col"))
  expect_identical(
    sort(res),
    sort(invisible(c("list_col", "complex_col", "raw_col")))
  )
})

test_that("check_var_class handles NULL column values", {
  dt <- data.table(a = 1:3, b = letters[1:3])

  # Note: In data.table, setting a column to NULL removes it
  # So this test documents that expected behavior
  # The NULL check in check_var_class is defensive programming
  # for edge cases where dt[[v]] might return NULL

  # Verify normal behavior - NULL removes column
  dt_copy <- copy(dt)
  suppressWarnings(dt_copy$null_col <- NULL)
  expect_false("null_col" %in% names(dt_copy))
})

test_that("check_var_class stores correct warning messages", {
  dt <- data.table(
    valid = 1:3,
    invalid = list(1, 2, 3)
  )

  clear_joynenv()
  check_var_class(dt, "invalid")

  msgs <- rlang::env_get(.joynenv, "joyn_msgs")

  # Check message type is warning
  expect_equal(msgs$type, "warn")

  # Check message contains variable name and class info
  expect_true(grepl("invalid", msgs$msg))
  expect_true(grepl("class", msgs$msg, ignore.case = TRUE))
  expect_true(grepl("list", msgs$msg))
})

test_that("check_var_class works with mixed valid and invalid variables", {
  dt <- data.table(
    good1 = 1:3,
    good2 = letters[1:3],
    bad1 = list(1, 2, 3),
    good3 = factor(c("a", "b", "c")),
    bad2 = as.raw(1:3)
  )

  clear_joynenv()
  res <- check_var_class(dt, c("good1", "bad1", "good2", "bad2", "good3"))

  # Should return only the bad variables
  expect_identical(sort(res), sort(invisible(c("bad1", "bad2"))))

  # Should have stored warnings
  expect_true(rlang::env_has(.joynenv, "joyn_msgs"))
  msgs <- rlang::env_get(.joynenv, "joyn_msgs")
  expect_equal(length(msgs$msg), 2)
})

test_that("check_var_class handles fs_path class", {
  skip_if_not_installed("fs")

  dt <- data.table(
    a = 1:3,
    path_col = fs::path(c("/path/to/file1", "/path/to/file2", "/path/to/file3"))
  )

  clear_joynenv()
  # fs_path is an allowed class
  expect_null(check_var_class(dt, "path_col"))
})
