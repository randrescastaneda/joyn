# Test update_values function ####


x1 <- data.table(id = c(1, 2, 5, 6),
                 x = c(NA, NA, NA, NA))

y1 <- data.table(id = c(1, 2, 3, 4),
                 x = c(10, 20, 30, 40))

x2 = data.table(id = c(1, 1, 2, 3, NA),
                 t = c(1L, 2L, 1L, 2L, NA_integer_),
                 x = c(16, 12, 10, NA, 15),
                y = c(NA, 2, 3, 4, 5))

y2 = data.table(id = c(1, 2, 5, 6, 3),
                yd = c(1, 2, 5, 6, 3),
                y  = c(11L, 15L, 20L, 13L, 10L),
                x  = c(NA, 17:20))

df1 <- data.frame(
  id = c(2, 3, 4, 5, 6, 7),
  x = c(100, 200, NA, 400, NA, NA),
  flag = c(TRUE, TRUE, FALSE, NA, TRUE, FALSE)
)

df2 <- data.frame(
  id = c(1, 2, 3, 4, 6, 3),
  x = c(10, 20, 30, 40, 50, 60),
  category = c("A", "B", "C", "D", "E", "F")
)


# Testing the function when data tables are used
dt = joyn(x2,
            y2,
            by               = "id",
            match_type       = "m:1",
            update_NAs       = FALSE,
            update_values    = FALSE,
            keep_common_vars = TRUE,
            reporttype       = "numeric",
            verbose          = FALSE)

test_that("update_values of one variable", {


  # Updating values in x2$x with values from y2$x
  res <- update_values(dt,
                       var       = "x",
                       reportvar = ".joyn")

  # check updated values
  which(res$.joyn == 5) |>
    expect_equal(which( !is.na(dt$x.x) & dt$.joyn == 3 & !is.na(dt$x.y)))

  # Check replaced values
  res$x.x |>
    fsubset(which(res$.joyn == 5)) |>
    expect_equal(res$x.y |> fsubset(which(res$.joyn == 5))
                   )

  # Check not updated values
  dt[is.na(x.x) | is.na(x.y) | !.joyn == 3] |> fselect((id:x.y)) |>
    expect_equal(res[!.joyn == 5,] |> fselect((id:x.y)))

  # Check x is not updated with NA values in x.y
  rows_y_na <- which(is.na(dt$x.y))

  res$x.x[rows_y_na] |>
    expect_equal(dt$x.x[rows_y_na])

})


test_that("update values of multiple variables", {
  res <- update_values(dt,
                       var       = c("x", "y"),
                       reportvar = ".joyn")

  # Check X
  # check not updated values
  which(!res$.joyn == 5) |>
    expect_equal(which(is.na(dt$x.x) |
                         is.na(dt$y.x) |
                         dt$.joyn == 1 |
                         is.na(dt$y.y) |
                         is.na(dt$x.y)))


  dt[!is.na(x.x) & .joyn == 4, x.x] |>
    expect_equal(dt[!is.na(x.y) & .joyn == 4, x.y])

  # Check x is not updated with NA values in x.y
  rows_y_na <- which(is.na(dt$x.y))

  res$x.x[rows_y_na] |>
    expect_equal(dt$x.x[rows_y_na])

  # Check y
  # check not updated values
  which(is.na(res$y.x) & res$.joyn %!in% c(2, 6)) |>
    expect_equal(which( is.na(dt$y.x) & dt$.joyn == 1))


  dt[!is.na(y.x) & .joyn == 4, y.x] |>
    expect_equal(dt[!is.na(y.y) & .joyn == 4, y.y])

  # Check x is not updated with NA values in x.y
  rows_y_na <- which(is.na(dt$y.y))

  res$y.x[rows_y_na] |>
    expect_equal(dt$y.x[rows_y_na])

})
