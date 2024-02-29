
# Test update_NAs function ####

# Sample data tables

x2 = data.table(id = c(1, 1, 2, 3, NA),
                 t = c(1L, 2L, 1L, 2L, NA_integer_),
                 x = c(16, 12, NA, NA, 15))

y2 = data.table(id = c(1, 2, 5, 6, 3),
                yd = c(1, 2, 5, 6, 3),
                y  = c(11L, 15L, 20L, 13L, 10L),
                x  = c(16:20))

# Sample data frames

data_frame_1 <- data.frame(
  id = c(2, 3, 4, 5, 6, 7),
  x = c(100, 200, NA, 400, NA, NA),
  flag = c(TRUE, TRUE, FALSE, NA, TRUE, FALSE)
)

data_frame_2 <- data.frame(
  id = c(1, 2, 3, 4, 6, 3),
  x = c(10, 20, 30, 40, 50, 60),
  category = c("A", "B", "C", "D", "E", "F")
)

data_frame_3 <- data.frame(
  id = c(2, 3, 4, 5, 6, 7),
  x = c(NA, NA, NA, NA, NA, NA),
  flag = c(TRUE, TRUE, FALSE, NA, TRUE, FALSE)
)

data_frame_4 <- data.frame(
  id = c(1, 2, 3, 4, 6, 3),
  x = c(100, 200, 300, 400, 500, 600),
  category = c("A", "B", "C", "D", "E", "F")
)


test_that("update_NAs works as expected -with data tables", {

  # Joined data frame with NA values in x2$x not updated
  dt = joyn(x2,
            y2,
            by         = "id",
            match_type = "m:1",
            keep_common_vars = TRUE)

  # Updating NAs in x with values from x.y
  res <- update_NAs(dt,
                    var       = "x",
                    reportvar = ".joyn")

  # Check replaced values
  any(is.na(res$x.x)) |>
      expect_equal(FALSE)

  res$x.x[4:7] |>
      expect_equal(dt[4:7, x.y])




  # Check when all x.var values are NAS

  # Check no change to data when no NAs in x.var

  # Check with other types of vars

  #

  # Check output class


})

test_that("update_NAs works as expected - with data frames", {

  # With data frames
  df = joyn(data_frame_1,
            data_frame_2,
            by         = "id",
            match_type = "1:m",
            keep_common_vars = TRUE)

  res_df <- update_NAs(df,
                       var       = "x",
                       reportvar = ".joyn")

  # Check all NAs are replaced -with values from y
  subset_res_df <- res_df[which(!is.na(df$x.y)), "x.x"]
  expect_false(any(is.na(subset_res_df)))




  # Check updated values TODO!!
  # take NA positions in x
  # take values in y at NA positions
  # check those positions in output

  # Check output class
  class(res_df) |>
    expect_equal("data.frame")

  # Check when all x.var values are NAS

  df_1 = joyn(data_frame_3,
            data_frame_4,
            by         = "id",
            match_type = "1:m",
            keep_common_vars = TRUE)


  res_df_1 <- update_NAs(df_1,
                       var       = "x",
                       reportvar = ".joyn")

  res_df_1$x.x |>
    expect_equal(res_df_1$x.y)

  # Check when no NAs to update
  df_2 <- joyn(data_frame_2,
               data_frame_4,
               by         = "id",
               match_type = "m:m",
               keep_common_vars = TRUE)

  res_df_2 <-  update_NAs(df_2,
                          var       = "x",
                          reportvar = ".joyn")

  res_df_2$x.x |>
    expect_equal(df_2$x.x)



})

test_that("update_NAs - input when data table", {

    # suffix

   dt = joyn(x2,
             y2,
             by         = "id",
             match_type = "m:1",
             keep_common_vars = TRUE)

   dt_1 = joyn(x2,
               y2,
               by         = "id",
               match_type = "m:1",
               keep_common_vars = TRUE,
               suffix = c(".x", ".y"))

   update_NAs(dt,
              var = "x",
              reportvar = ".joyn") |>
      expect_equal(

        update_NAs(dt_1,
                   var = "x",
                   reportvar = ".joyn")
      )

   # missing reportvar in joined table
   dt_2 = joyn(x2,
             y2,
             by         = "id",
             match_type = "m:1",
             keep_common_vars = TRUE,
             reportvar = FALSE)

   update_NAs(dt_2,
              var = "x") |>
     expect_error()

   # missing var specificied
   update_NAs(dt_2,
              var = "yd") |>
     expect_error()

})

test_that("update_NAs - input when no data table", {

  # suffix

  df = joyn(data_frame_1,
            data_frame_2,
            by         = "id",
            match_type = "1:m",
            keep_common_vars = TRUE)

  df_1 = joyn(data_frame_1,
              data_frame_2,
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
  df_2 = joyn(data_frame_1,
              data_frame_2,
              by         = "id",
              match_type = "1:m",
              keep_common_vars = TRUE,
              reportvar = FALSE)

  update_NAs(df_2,
             var = "x") |>
    expect_error()

  # missing var specificied
  update_NAs(df_2,
             var = "yd") |>
    expect_error()


})

