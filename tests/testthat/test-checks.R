
# Testing functions performing checks on x and y ####

#withr::local_options(joyn.verbose = FALSE)


x1 = data.frame(
  id = c(1L, 1L, 2L, 3L, NA_integer_),
  t  = c(1L, 2L, 1L, 2L, NA_integer_),
  x  = 11:15
)

x1_duplicates = data.frame(
  id = c(1L, 1L, 2L, 3L, NA_integer_),
  x  = c(1L, 2L, 1L, 2L, NA_integer_),
  x  = 11:15,
  check.names = FALSE
)

y1 = data.table(id = c(1,2, 4),
                y  = c(11L, 15L, 16))

x2 = data.frame(
  id = c(1, 1, 2, 3, NA),
  t  = c(1L, 2L, 1L, 2L, NA_integer_),
  x  = c(16, 12, NA, NA, 15)
)

x3 = data.frame(
      id  = c(1, 2, 3, 4, 5),
       t  = c(1L, 2L, 5L, 6L, NA_integer_),
       x  = c(16, 12, NA, NA, 15)
   )

y2 = data.frame(
  id = c(1, 2, 5, 6, 3),
  yd = c(1, 2, 5, 6, 3),
  y  = c(11L, 15L, 20L, 13L, 10L),
  x  = c(16:20)
)

y3 = data.frame(
       id = c(1, 2, 2, 6, 3),
       yd = c(1, 2, 5, 6, 3),
       y  = c(11L, 15L, 20L, 13L, 10L),
       x  = c(16:20)
   )

df1 <- data.frame(
  id1 = c(1, 1, 2, 3),
  id2 = c("a", "b", "b", "c"),
  name = c("John", "Jane", "Bob", "Carl"),
  age = c(35, 28, 42, 50)
)

df2 <- data.frame(
  id1 = c(1, 2, 3, 3),
  id2 = c("a", "b", "c", "e"),
  salary = c(60000, 55000, 70000, 80000),
  dept = c("IT", "Marketing", "Sales", "IT")
)


# Test function checking x and y ---------------------------------------------------------------
test_that("check_xy works as expected", {

  # Errors
  check_xy(x = x1) |>
    expect_error()

  check_xy(y = y1) |>
    expect_error()

  # Checking that msgs stored in env when error exists, and that the function
  # aborts when wrong input specification

  # When x has 0 length

  empty_df = data.frame()

  check_xy(x = empty_df,
           y = y1) |>
    expect_error()

  expect_true(rlang::env_has(.joynenv,
                             "joyn_msgs"))

  # When y has 0 length

  check_xy(x = x1,
           y = empty_df) |>
      expect_error()

  expect_true(rlang::env_has(.joynenv,
                             "joyn_msgs"))

  # When both x and y have 0 length
  clear_joynenv()

  check_xy(x = empty_df,
           y = empty_df) |>
      expect_error()

  expect_true(rlang::env_has(.joynenv,
                             "joyn_msgs"))

  # No msg When no duplicate names
  clear_joynenv()

  check_xy(x = x1,
           y = y1)

  expect_false(rlang::env_has(.joynenv,
                              "joyn_msgs"))

  # when duplicate names

  clear_joynenv()

  check_xy(x = x1_duplicates,
           y = y1) |>
    expect_error()

  expect_true(rlang::env_has(.joynenv,
                             "joyn_msgs"))

  # ==============================
  # Tests for 0 rows
  # ==============================

  x0_rows <- x1[0, ]
  y0_rows <- y1[0, ]

  # x has 0 rows
  clear_joynenv()
  check_xy(x = x0_rows, y = y1) |>
    expect_error()
  expect_true(rlang::env_has(.joynenv, "joyn_msgs"))

  # y has 0 rows
  clear_joynenv()
  check_xy(x = x1, y = y0_rows) |>
    expect_error()
  expect_true(rlang::env_has(.joynenv, "joyn_msgs"))

  # Both x and y have 0 rows
  clear_joynenv()
  check_xy(x = x0_rows, y = y0_rows) |>
    expect_error()
  expect_true(rlang::env_has(.joynenv, "joyn_msgs"))

})

# Testing function checking duplicate var names in dt ---------------------------------------
test_that("check_duplicate_names works as expected", {

  # Check output when duplicates
  check_duplicate_names(x1_duplicates, "x") |>
      expect_equal(TRUE)

  # Check output When no duplicates
  check_duplicate_names(x1, "x") |>
      expect_equal(FALSE)

  # Check msg is stored when duplicates
  clear_joynenv()

  check_duplicate_names(x1_duplicates, "x")

  expect_true(rlang::env_has(.joynenv, "joyn_msgs"))


})

# Test function checking reportvar ---------------------------------------------------------
test_that("check_reportvar works as expected", {

  # Inputs
  check_reportvar(reportvar = NULL) |>
    expect_no_error()

  check_reportvar(reportvar = FALSE) |>
    expect_no_error()

  check_reportvar(reportvar = 2) |>
    expect_error()

  # Output

  check_reportvar(reportvar = NULL) |>
    expect_equal(check_reportvar(reportvar = FALSE))

  check_reportvar(reportvar = FALSE) |>
    expect_equal(NULL)

  # Valid name
  check_reportvar(".joyn") |>
    expect_equal(".joyn")

})

# Test function checking `by` input ---------------------------------------------------------------

test_that("check_by_vars function works as expected", {

  # Output
  res <- check_by_vars(by = 'id', x = x1, y = y1)

  class(res) |>
    expect_equal("list")

  names(res) |>
    expect_equal(c("by", "xby", "yby", "tempkey"))

  res$by |>
    expect_equal("id")

  # Check it throws an error when by is NULL
  check_by_vars(by = NULL, x = x1, y = y1) |>
    expect_error()

})

# Test function checking match type consistency -------------------------------------------------
test_that("check_match_type works as expected", {

  # Inputs
  check_match_type(x1, y1, by= NULL) |>
    expect_error()

  check_match_type(x1, y1, by = "id", match_type = "invalid match_type") |>
    expect_error()

  # Wrong by var
  check_match_type(x4, y2, by = 'id', match_type = "m:m") |>
    expect_error()


  # Outputs - Errors & Warnings

  # If user chooses "1:1" but actually "m:1" -----------------------------------
  clear_joynenv()

  check_match_type(x1, y1, by = "id", match_type = "1:1") |>
    expect_error()

  # check error msg is stored in env
  rlang::env_get(.joynenv, "joyn_msgs")$type |>
      expect_equal("err")

  # If user chooses "1:1" but actually "1:m" -----------------------------------
  clear_joynenv()

  check_match_type(x3, y3, by = "id", match_type = "1:1") |>
    expect_error()

  # check error msg is stored in env
  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_equal("err")

  # If user chooses "1:1" but actually "m:m" -----------------------------------
  clear_joynenv()

  check_match_type(df1, df2, by = "id1", match_type = "1:1") |>
    expect_error()

  # check error msg is stored in env
  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_equal(c("err", "err"))

  # If user chooses "m:1" but it is actually "m:m" -----------------------------
  clear_joynenv()

  check_match_type(x2, y3, by = 'id', match_type = "m:1") |>
    expect_error()

  # check err msg is stored in env
  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_equal(c("err"))

  # If user chooses "m:1" but it is actually "1:m" -----------------------------
  clear_joynenv()
  check_match_type(x3, y3, by = 'id', match_type = "m:1") |>
    expect_error()

  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_equal(c("err"))


  # Check warning msgs are stored correctly ------------------------------------
  # Warn x
  check_match_type(x3, y3, by = 'id', match_type = "m:m")

  rlang::env_get(.joynenv,"joyn_msgs")$type|>
    expect_contains("warn")

  clear_joynenv()
  check_match_type(x3, y2, by = 'id', match_type = "m:1")

  rlang::env_get(.joynenv,"joyn_msgs")$type|>
    expect_contains("warn")


  # Warn y
  clear_joynenv()
  check_match_type(x2, y2, by = 'id', match_type = "m:m", verbose = TRUE)

  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_equal(c("warn"))

  clear_joynenv()
  check_match_type(x3, y2, by = 'id', match_type = "1:m", verbose = TRUE)

  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_equal(c("warn"))

  # Warn both

  clear_joynenv()
  check_match_type(x3, y2, by = 'id', match_type = "m:m")

  rlang::env_get(.joynenv, "joyn_msgs")$type |>
    expect_equal(c("warn"))


  # Output when correct match type ---------------------------------------------
  check_match_type(x3, y2, by = "id", match_type = "1:1") |>
      expect_equal(c("1", "1"))

  check_match_type(df1, df2, by = "id1", match_type = "m:m") |>
      expect_equal(c("m", "m"))

  check_match_type(x1, y1, by = "id", match_type = "m:1") |>
    expect_equal(c("m", "1"))

  check_match_type(x3, y2, by = "id", match_type = "1:m") |>
    expect_equal(c("1", "m"))

  # Output class
  class(check_match_type(x1, y1, by = 'id', match_type = "m:1")) |>
    expect_equal("character")

})

# Test function confirming if match_type_error ####

test_that("is_match_type_error works as expected", {
  clear_joynenv()

  # Note (Rossana): to check if this is the correct use
  res <- is_match_type_error(x1, "x", by = "id", match_type_error = TRUE)

  res |>
    expect_equal(TRUE)

  expect_true(rlang::env_has(.joynenv,
                             "joyn_msgs"))

})

# Test function checking vars in Y kept in output table -----------------------------------------------------------------------------
test_that("y_vars_to_keep checks work", {
  ## var no available ---------
  y_vars_to_keep <- "f"
  by             <- "id"
  check_y_vars_to_keep(y_vars_to_keep, y1, by) |>
    expect_error()

  y_vars_to_keep <- c("F", "r")
  check_y_vars_to_keep(y_vars_to_keep, y1, by) |>
    expect_error()

  ## more than one value when no string --------
  y_vars_to_keep <- c(TRUE, TRUE)
  check_y_vars_to_keep(y_vars_to_keep, y1, by) |>
    expect_error()

  ## something besides character, false, or NULL ---------
  check_y_vars_to_keep(NA, y1, by) |>
    expect_error()

  # Output -------------
  check_y_vars_to_keep(NULL, y1, by) |>
    expect_null()

  check_y_vars_to_keep(FALSE, y1, by) |>
    expect_null()

  check_y_vars_to_keep("y", y1, by) |>
    expect_equal("y")

  check_y_vars_to_keep(c("id", "y"), y1, by) |>
    expect_equal("y")

  check_y_vars_to_keep(TRUE, y1, by) |>
    expect_equal("y")

})

# Testing function renaming the variables in y after joining
test_that("check_new_y_vars checks work", {

  ## no common names, return the same ---------
  check_new_y_vars(x = df1, by = "id1",
                   y_vars_to_keep = "salary") |>
    expect_equal("salary")

  ## when common name, add suffix -----------
  check_new_y_vars(x = df1, by = "id1",
                   y_vars_to_keep = c("id2", "salary")) |>
    expect_equal(c("id2.y", "salary"))

  class(check_new_y_vars(x = df1, by = "id1", y_vars_to_keep = "salary")) |>
    expect_equal("character")
})

# Testing the function that checks if specified "many" relationship is valid ####
test_that("is_valid_m_key works as expected", {

  is_valid_m_key(x1, by = 2) |>
    expect_error()

  is_valid_m_key(x1, by = "id") |>
    expect_equal(TRUE)

  is_valid_m_key(x1, by = c("id", "x")) |>
    expect_equal(FALSE)

})

test_that("check_var_class works as expected", {

  clear_joynenv()

  dt <- data.table(a = 1:3,
                   b = letters[1:3])

  # Allowed class: integer
  expect_null(check_var_class(dt,
                              "a"))

  # Allowed class: character
  expect_null(check_var_class(dt,
                              "b"))

  # Disallowed class: list
  dt$c <- list(1, 2, 3)

  clear_joynenv()
  res <- check_var_class(dt, "c")

  expect_identical(res,
                   invisible("c"))

  expect_true(rlang::env_has(.joynenv,
                             "joyn_msgs"))

  msg <- rlang::env_get(.joynenv,
                        "joyn_msgs")

  expect_true(grepl("class",
                    msg$msg))
})

