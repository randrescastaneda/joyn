
# Test update_NAs and/or values function

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

dt = joyn(x2,
          y2,
          by               = "id",
          match_type       = "m:1",
          update_NAs       = FALSE,
          update_values    = FALSE,
          keep_common_vars = TRUE,
          reporttype       = "numeric",
          verbose          = FALSE)

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

df3 <- data.frame(
  id = c(2, 3, 4, 5, 6, 7),
  x = c(NA, NA, NA, NA, NA, NA),
  flag = c(TRUE, TRUE, FALSE, NA, TRUE, FALSE)
)

df4 <- data.frame(
  id = c(1, 2, 3, 4, 6, 3),
  x = c(100, 200, 300, 400, 500, 600),
  category = c("A", "B", "C", "D", "E", "F")
)



# Testing output function when input is data table ####
test_that("update_na_values -update NAs only", {

  res <- update_na_values(dt = dt,
                          var       = "x",
                          reportvar = ".joyn",
                          rep_NAs = TRUE,
                          rep_values = FALSE)

  # check update of reportvar
  which(res$.joyn == 4) |>
    expect_equal(which( is.na(dt$x.x) & !is.na(dt$x.y)))

  # check updated values
  res$x.x[5:7] |>
    expect_equal(dt[5:7, x.y])

  # check output class
  inherits(res, "data.table") |>
    expect_equal(TRUE)

})

test_that("update_na_vals -update values of one var", {

  res <- update_na_values(dt = dt,
                          var       = "x",
                          reportvar = ".joyn",
                          rep_NAs = FALSE,
                          rep_values = TRUE)

  # check update_values
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

  # check output class
  inherits(res, "data.table") |>
    expect_equal(TRUE)

})

test_that("update_na_values -update values of more than one var", {
  res <- update_na_values(dt = dt,
                          var       = c("x", "y"),
                          reportvar = ".joyn",
                          rep_values = TRUE)

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

  # check output class
  inherits(res, "data.table") |>
    expect_equal(TRUE)

})

test_that("update_na_values -update both NAs and values", {
  res <- update_na_values(dt = dt,
                          var       = "x",
                          reportvar = ".joyn",
                          rep_NAs = TRUE,
                          rep_values = TRUE)
  res$x.x |>
    fsubset(which(res$.joyn == 5)) |>
    expect_equal(res$x.y |> fsubset(which(res$.joyn == 5))
    )

  res$x.x |>
    fsubset(which(res$.joyn == 4)) |>
    expect_equal(res$x.y |> fsubset(which(res$.joyn == 4))
    )

  # check output class
  inherits(res, "data.table") |>
    expect_equal(TRUE)
})

# Testing the function with non data table input ####
test_that("update_na_values -update NAs only", {

  df <- joyn(df1,
             df2,
             by               = "id",
             match_type       = "1:m",
             keep_common_vars = TRUE,
             update_NAs       = FALSE,
             update_values    = FALSE,
             reporttype       = "numeric")

  res <- update_na_values(df,
                          var       = "x",
                          rep_NAs = TRUE)

  # Check all NAs are replaced -with values from y
  to_replace <- res[which(!is.na(df$x.y)), "x.x"] #rows that are NAs in x.x but not NA in x.y
  expect_false(any(is.na(to_replace)))

  which(res$.joyn == 4) |>
    expect_equal(which(is.na(df$x.x) &
                         !is.na(df$x.y)))

  which(res$.joyn != 4) |>
    expect_equal(which(!is.na(df$x.x)) |>
                   append(which(is.na(df$x.x) & is.na(df$x.y))))

  })



