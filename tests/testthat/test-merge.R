withr::local_options(joyn.verbose = FALSE)
library(data.table) |>
  suppressWarnings()

# Load DATA --------------------------------------------------------------------

# options(joyn.verbose = FALSE)
x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = 11:15)

y1 = data.table(id = c(1,2, 4),
                y  = c(11L, 15L, 16))


x2 = data.table(id = c(1, 4, 2, 3, NA),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = c(16, 12, NA, NA, 15))


y2 = data.table(id = c(1, 2, 5, 6, 3),
                yd = c(1, 2, 5, 6, 3),
                y  = c(11L, 15L, 20L, 13L, 10L),
                x  = c(16:20))


y3 <- data.table(id = c("c","b", "c", "a"),
                 y  = c(11L, 15L, 18L, 20L))

x3 <- data.table(id  = c("c","b", "d"),
                 v   = 8:10,
                 foo = c(4,2, 7))

x4 = data.table(id1 = c(1, 1, 2, 3, 3),
                id2 = c(1, 1, 2, 3, 4),
                t   = c(1L, 2L, 1L, 2L, NA_integer_),
                x   = c(16, 12, NA, NA, 15))


y4 = data.table(id  = c(1, 2, 5, 6, 3),
                id2 = c(1, 1, 2, 3, 4),
                y   = c(11L, 15L, 20L, 13L, 10L),
                x   = c(16:20))
x5 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_, NA_integer_),
                t  = c(1L, 2L, 1L, 2L, NA_integer_, 4L),
                x  = 11:16)

y5 = data.table(id = c(1,2, 4, NA_integer_, NA_integer_),
                y  = c(11L, 15L, 16, 17L, 18L))

reportvar = getOption("joyn.reportvar")

# Warnings and errors ---------------

test_that("warnings and erros are triggered correctly", {
  merge(
    x = x1,
    y = y1,
    match_type = "m:1",
    allow.cartesian = TRUE,
    by = "id"
  ) |> expect_warning()


  merge(
    x = x1,
    y = y1,
    match_type = "1:1",
    all.x = TRUE,
    by = "id",
    reportvar = FALSE
  ) |>
    expect_error(label = "merge did not detect inconsistency in match type 1:1")


})


# TEST LEFT JOINS --------------------------------------------------------------

test_that("LEFT JOIN - Conducts left join", {
  # m:1  -----------
  # One way
  jn_joyn <- merge(
    x = x1,
    y = y1,
    match_type = "m:1",
    all.x = TRUE,
    by = "id"
  )


  jn_dt <- merge.data.table(x = x1,
                             y = y1,
                             match_type = "m:1",
                             all.x = TRUE,
                             by = "id")

  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dt
  )

  expect_equal(
    merge(
      x = x1,
      y = y1,
      match_type = "m:1",
      all.x = TRUE,
      by = "id",
      reportvar = FALSE
    ),
    jn_dt
  )


  # 1:1 ----------------------
  jn_joyn <- merge(
    x = x2,
    y = y2,
    match_type = "1:1",
    all.x = TRUE,
    by = "id"
  )

  jn_dt <- merge.data.table(
    x = x2,
    y = y2,
    all.x = TRUE,
    by = "id"
  )


  expect_equal(
    jn_joyn |>
      fselect(-get(reportvar)), # `jvar` should be `.joyn` in principle
    jn_dt
  )


  # m:m  ----------------------
  jn <- merge(
    x4,
    y4,
    match_type = "m:m",
    all.x = TRUE,
    by.x = "id1",
    by.y = "id2"
  )

  jn_dt <-  merge.data.table(
    x4,
    y4,
    # match_type = "m:m",
    all.x = TRUE,
    by.x = "id1",
    by.y = "id2"
  )

  # jn_dt <- data.table(id1 = c(1, 1, 1, 1, 2, 3, 3),
  #                        id2 = c(1, 1, 1, 1, 2, 3, 4),
  #                        t   = c(1, 1, 2, 2, 1, 2, NA),
  #                        x.x = c(16, 16, 12, 12, NA, NA, 15),
  #                        id  = c(1, 2, 1, 2, 5, 6, 6),
  #                        y   = c(11, 15, 11, 15, 20, 13, 13),
  #                        x.y = c(16, 17, 16, 17, 18, 19, 19))

  attr(jn_dt, "sorted") <- "id1"
  expect_equal(
    jn |> fselect(-get(reportvar)),
    jn_dt,
    ignore_attr = ".internal.selfref"
  )

})
