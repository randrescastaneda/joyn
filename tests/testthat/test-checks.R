# Load helper data
data <- make_test_data()

# Extract datasets
x1            <- data$x1
x1_duplicates <- data$x1_duplicates
x2            <- data$x2
x3            <- data$x3
y1            <- data$y1
y2            <- data$y2
y3            <- data$y3
df1           <- data$df1
df2           <- data$df2

# ----------------- TESTS -----------------

test_that("check_xy works as expected", {

  empty_df <- data.frame()

  # Errors
  expect_error(
    check_xy(x = x1)
  )
  expect_error(
    check_xy(y = y1)
  )
  expect_error(
    check_xy(x = empty_df, y = y1)
  )
  expect_error(
    check_xy(x = x1, y = empty_df)
  )

  clear_joynenv()
  expect_error(
    check_xy(x = empty_df, y = empty_df)
  )

  # No msg when no duplicate names
  clear_joynenv()
  expect_no_error(
    check_xy(x = x1, y = y1)
  )

  # Duplicate names
  clear_joynenv()
  expect_error(
    check_xy(x = x1_duplicates, y = y1)
  )

  # 0 rows tests
  x0_rows <- x1[0, ]
  y0_rows <- y1[0, ]

  clear_joynenv()
  expect_error(
    check_xy(x = x0_rows, y = y1)
  )
  clear_joynenv()
  expect_error(
    check_xy(x = x1, y = y0_rows)
  )
  clear_joynenv()
  expect_error(
    check_xy(x = x0_rows, y = y0_rows)
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
    x  = x1,
    y  = y1
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
    x          = x3,
    y          = y3,
    by         = "id",
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
      x             = df1,
      by            = "id1",
      y_vars_to_keep = "salary"
    ),
    "salary"
  )

  expect_equal(
    check_new_y_vars(
      x             = df1,
      by            = "id1",
      y_vars_to_keep = c("id2", "salary")
    ),
    c("id2.y", "salary")
  )

  expect_equal(
    class(
      check_new_y_vars(
        x             = df1,
        by            = "id1",
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
    a = 1:3,                     # integer
    b = letters[1:3],            # character
    d = as.IDate(Sys.Date())     # inherits from "Date"
  )

  # All valid classes
  expect_null(check_var_class(dt, c("a", "b", "d")))

  # Add a list column (unsupported)
  dt$c <- list(1, 2, 3)
  clear_joynenv()
  res <- check_var_class(dt, "c")

  # Function should return invisible("c")
  expect_identical(res, invisible("c"))

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
  expect_identical(sort(res_multi), sort(invisible(c("c", "e"))))
})
