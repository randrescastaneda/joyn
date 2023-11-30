x1 = data.frame(
  id = c(1L, 1L, 2L, 3L, NA_integer_),
  t  = c(1L, 2L, 1L, 2L, NA_integer_),
  x  = 11:15
)

y1 = data.frame(id = 1:2,
                y  = c(11L, 15L))

x2 = data.frame(
  id = c(1, 1, 2, 3, NA),
  t  = c(1L, 2L, 1L, 2L, NA_integer_),
  x  = c(16, 12, NA, NA, 15)
)

y2 = data.frame(
  id = c(1, 2, 5, 6, 3),
  yd = c(1, 2, 5, 6, 3),
  y  = c(11L, 15L, 20L, 13L, 10L),
  x  = c(16:20)
)


test_that("y_vars_to_keep checks work", {
  # errors ---------
  #
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

})
