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
  check_y_vars_to_keep(y_vars_to_keep, y1) |>
    expect_error()

  y_vars_to_keep <- c("F", "r")
  check_y_vars_to_keep(y_vars_to_keep, y1) |>
    expect_error()

  ## more than one value when no string --------
  y_vars_to_keep <- c(TRUE, TRUE)
  check_y_vars_to_keep(y_vars_to_keep, y1) |>
    expect_error()


})
