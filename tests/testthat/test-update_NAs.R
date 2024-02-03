
# Test update_NAs function ####

x2 = data.table(id = c(1, 1, 2, 3, NA),
                 t = c(1L, 2L, 1L, 2L, NA_integer_),
                 x = c(16, 12, NA, NA, 15))

y2 = data.table(id = c(1, 2, 5, 6, 3),
                yd = c(1, 2, 5, 6, 3),
                y  = c(11L, 15L, 20L, 13L, 10L),
                x  = c(16:20))


test_that("update_NAs works as expected", {

  # Joined data frame with NA values in x2$x not updated
  dt = joyn(x2,
            y2,
            by         = "id",
            match_type = "m:1",
            keep_common_vars = TRUE)

  colnames(dt)[3] = "x"

  # Updating NAs in x with values from x.y
  res <- update_NAs(dt,
                    var       = "x",
                    reportvar = ".joyn")

  # Check replaced values
  any(is.na(res$x)) |>
      expect_equal(FALSE)

  values = dt[4:7, x.y]

  res$x[4:7] |>
      expect_equal(values)
})


