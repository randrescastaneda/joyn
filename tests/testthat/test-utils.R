
# Testing utils functions

x1 = data.frame(id  = c(1L, 1L, 2L, 3L, NA_integer_),
                t   = c(1L, 2L, 1L, 2L, NA_integer_),
                x   = 11:15)

y1 = data.frame(id  = c(1L, 2L, 3L, 1L, 2L, 3L),
                t   = c(1L, 1L, 1L, 2L, 2L, 2L),
                x   = 11:16)

# Rename to valid function ####
test_that("rename_to_valid works as expected", {

  # Errors
  rename_to_valid(name = 5L) |>
      expect_error()

  rename_to_valid(name = TRUE) |>
    expect_error()

  # Valid name
  rename_to_valid("x-") |>
    expect_equal("x.")
  rename_to_valid("") |>
    expect_equal("X")
  # rename_to_valid("x-") |>
  #   expect_message()
  #
  # rename_to_valid("") |>
  #   expect_message()

})

# Split matching type function ####
test_that("split_match_type works as expected", {

  match_types <- c("1:1", "m:1", "1:m", "m:m")

  split_match_type("1:1") |>
      expect_equal(c("1", "1"))

  split_match_type("1:m") |>
    expect_equal(c("1", "m"))

  split_match_type("m:1") |>
    expect_equal(c("m", "1"))

  split_match_type("m:m") |>
    expect_equal(c("m", "m"))

  split_match_type("2:1") |>
    expect_error()

})

# is_balance function ####
test_that("is_balanced inputs", {

  # Errors when incorrect inputs
  is_balanced(df = x1,
              by = TRUE) |>
    expect_error()

  is_balanced(df = x1,
              by = "y") |>
    expect_error()

  is_balanced(df = x1,
              by = c("x", "y")) |>
    expect_error()

})

test_that("is_balanced outputs", {

  by = c("id", "t")

  # unbalanced data.frame
  is_balanced(x1,
              by) |>
      expect_equal(FALSE)

  res_table <- is_balanced(x1,
                           by,
                           return = "table")

  class(res_table) |>
    expect_equal("data.frame")

  ncol(res_table) |>
      expect_equal(2)

  res_table$id |>
      expect_equal(c(3, 2))

  res_table$t |>
    expect_equal(c(1, 2))

  # balanced data.frame
  is_balanced(y1,
              by) |>
    expect_equal(TRUE)

  is_balanced(y1,
              by,
              return = "table")$id |>
    expect_length(0)

  is_balanced(y1,
              by,
              return = "table")$t |>
    expect_length(0)


})


