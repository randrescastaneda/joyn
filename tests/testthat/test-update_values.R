# Test update_values function ####

x2 = data.table(id = c(1, 1, 2, 3, NA),
                 t = c(1L, 2L, 1L, 2L, NA_integer_),
                 x = c(16, 12, NA, NA, 15))

y2 = data.table(id = c(1, 2, 5, 6, 3),
                yd = c(1, 2, 5, 6, 3),
                y  = c(11L, 15L, 20L, 13L, 10L),
                x  = c(16:20))

test_that("update_values works as expected", {

  # Joined data frame with values in x2 vars not updated
  dt = joyn(x2,
            y2,
            by         = "id",
            match_type = "m:1",
            update_NAs = FALSE)

  colnames(dt)[3] = "x"

  # Updating values in x2$x with values from y2$x
  res <- update_values(dt,
                       var       = "x",
                       reportvar = ".joyn")

  # Check updated values

  any(is.na(res$x)) |>
    expect_equal(FALSE)

  rows_y_not_na <- which(!is.na(dt$x.y))

  res$x[rows_y_not_na] |>
      expect_equal(dt$x.y[rows_y_not_na])

  # Check x is not updated with NA values in x.y
  rows_y_na <- which(is.na(dt$x.y))

  res$x[rows_y_na] |>
    expect_equal(dt$x[rows_y_na])

})
