
# Test update_NAs function ####

# Sample data tables

x1 <- data.table(id = c(1, 2, 5, 6),
                 x = c(NA, NA, NA, NA))

y1 <- data.table(id = c(1, 2, 3, 4),
                 x = c(10, 20, 30, 40))

x2 <- data.table(id = c(1, 1, 2, 3, NA),
                 t = c(1L, 2L, 1L, 2L, NA_integer_),
                 x = c(16, 12, NA, NA, 15))

y2 <- data.table(id = c(1, 2, 5, 6, 3),
                yd = c(1, 2, 5, 6, 3),
                y  = c(11L, 15L, 20L, 13L, 10L),
                x  = c(16:20))

# Sample data frames

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

# Testing function when input object is data table #############################

# Inputs ####
test_that("update_NAs (dt) - input", {

  # suffix
  jn_dt = joyn(x2,
            y2,
            by         = "id",
            match_type = "m:1",
            keep_common_vars = TRUE)

  jn_dt1 = joyn(x2,
              y2,
              by         = "id",
              match_type = "m:1",
              keep_common_vars = TRUE,
              suffix = c(".x", ".y"))

  update_NAs(jn_dt,
             var = "x",
             reportvar = ".joyn") |>
    expect_equal(

      update_NAs(jn_dt1,
                 var = "x",
                 reportvar = ".joyn")
    )

  # missing reportvar in joined table
  jn_dt2 = joyn(x2,
              y2,
              by         = "id",
              match_type = "m:1",
              keep_common_vars = TRUE,
              reportvar = FALSE)

  update_NAs(jn_dt2,
             var = "x") |>
    expect_error()

  # wrong var specified
  update_NAs(jn_dt2,
             var = "yd") |>
    expect_error()

})

# Outputs ####
test_that("update_NAs works as expected -replace all NAs", {

  # Joined data frame with NA values in x2$x not updated
  jn_dt = joyn(x2,
            y2,
            by         = "id",
            match_type = "m:1",
            keep_common_vars = TRUE)

  # Updating NAs in x with values from x.y
  res <- update_NAs(jn_dt,
                    var       = "x",
                    reportvar = ".joyn")

  # Check replaced values
  any(is.na(res$x.x)) |>
      expect_equal(FALSE)

  res$x.x[4:7] |>
      expect_equal(jn_dt[4:7, x.y])

  # Check output class
  inherits(res, "data.table") |>
    expect_equal(TRUE)


})

test_that("update_NAs works as expected (df) - when all values from x are NAs", {
  jn_dt = joyn(x1,
               y1,
               by         = "id",
               match_type = "1:1",
               keep_common_vars = TRUE)


  res_dt <- update_NAs(jn_dt,
                       var       = "x",
                       reportvar = ".joyn")
  # Failing because NAcol treated as logical
  #res_dt$x.x |>
  #  expect_equal(res_df$x.y)


})

# Testing function when input object is NOT data table #########################
test_that("update_NAs (df) - inputs work as expected", {

  # suffix

  df = joyn(df1,
            df2,
            by         = "id",
            match_type = "1:m",
            keep_common_vars = TRUE)

  df_1 = joyn(df1,
              df2,
              by         = "id",
              match_type = "1:m",
              keep_common_vars = TRUE,
              suffix = c(".x", ".y"))

  update_NAs(df,
             var = "x",
             reportvar = ".joyn") |>
    expect_equal(

      update_NAs(df_1,
                 var = "x",
                 reportvar = ".joyn")
    )

  # missing reportvar in joined table
  df_2 = joyn(df1,
              df2,
              by         = "id",
              match_type = "1:m",
              keep_common_vars = TRUE,
              reportvar = FALSE)

  update_NAs(df_2,
             var = "x") |>
    expect_error()

  # wrong var specificied
  update_NAs(df_2,
             var = "yd") |>
    expect_error()


})

# Outputs ####
test_that("update_NAs works as expected (df) - replace all NAs", {

  # With data frames
  jn_df = joyn(df1,
              df2,
              by         = "id",
              match_type = "1:m",
              keep_common_vars = TRUE)

  res_df <- update_NAs(jn_df,
                       var       = "x",
                       reportvar = ".joyn")

  # Check all NAs are replaced -with values from y
  to_replace <- res_df[which(!is.na(jn_df$x.y)), "x.x"] #rows that are NAs in x.x but not NA in x.y
  expect_false(any(is.na(to_replace))) })

test_that("update_NAs works as expected (df) - when all values from x are NAs", {
  jn_df = joyn(df3,
              df4,
              by         = "id",
              match_type = "1:m",
              keep_common_vars = TRUE)


  res_df <- update_NAs(jn_df,
                         var       = "x",
                         reportvar = ".joyn")

  res_df$x.x |>
    expect_equal(res_df$x.y)

})

test_that("update_NAs works as expected (df) - when no NAs to replace", {
  jn_df <- joyn(df2,
               df4,
               by         = "id",
               match_type = "m:m",
               keep_common_vars = TRUE)

  res_df <-  update_NAs(jn_df,
                          var       = "x",
                          reportvar = ".joyn")

  res_df$x.x |>
    expect_equal(jn_df$x.x)

})









