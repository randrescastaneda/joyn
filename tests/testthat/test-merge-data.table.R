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
    # x = x1,
    y = y1,
    match_type = "m:1",
    allow.cartesian = TRUE,
    by = "id"
  ) |> expect_error(label = "")


  merge(
    x = x1,
    y = y1,
    match_type = "1:1",
    all.x = TRUE,
    by = "id",
    reportvar = FALSE
  ) |>
    expect_error(label = "merge did not detect inconsistency in match type 1:1")


  merge(
    x = x2,
    y = y2,
    by = "id",
    match_type = "m:m",
    all.y = TRUE,
    reportvar = FALSE
  ) |>
    expect_error(label = "1:1 data was not found in m:m match type check")


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
      fselect(-get(reportvar)), # `reportvar` should be `.joyn` in principle
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


  expect_equal(
    jn |> fselect(-get(reportvar)),
    jn_dt
  )

})



# TEST RIGHT JOINS ------------------------------------------------------

test_that("RIGHT JOIN - Conducts right join", {

  # One way
  jn_joyn <- merge(
    x = x1,
    y = y1,
    match_type = "m:1",
    all.y = TRUE,
    by = "id"
  )


  jn_dt <- merge.data.table(
    x = x1,
    y = y1,
    all.y = TRUE,
    by = "id"
  )

  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dt
  )


  # Second set of tables ----------------------
  jn_joyn <- merge(
    x = x2,
    y = y2,
    match_type = "1:1",
    by = "id",
    all.y = TRUE
  )

  jn_dt <- merge.data.table(
    x2,
    y2,
    by = "id",
    all.y = TRUE
  )

  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dt
  )

  expect_equal(
    merge(
      x = x2,
      y = y2,
      # match_type = "1:1",
      by = "id",
      all.y = TRUE,
      reportvar = FALSE
    ),
    jn_dt
  )


  jn <- merge(
    x4,
    y4,
    by.x = "id1",
    by.y = "id2",
    match_type = "m:m",
    all.y = TRUE
  )

  jn_dt <- merge.data.table(
    x4,
    y4,
    by.x = "id1",
    by.y = "id2",
    match_type = "m:m",
    all.y = TRUE
  )

  expect_equal(
    jn |> fselect(-get(reportvar)),
    jn_dt
  )

})


# TEST FULL JOINS -------------------------------------------------------------

test_that("FULL JOIN - Conducts full join", {

  # One way
  jn_joyn <- merge(
    x = x1,
    y = y1,
    match_type = "m:1",
    by = "id"
  )


  jn_dt <- merge.data.table(
    x1,
    y1,
    by = "id",
  )
  setorder(jn_dt, na.last = T)
  attr(
    jn_dt,
    "sorted"
  ) <- "id"

  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dt
  )
  expect_equal(
    jn_joyn,
    jn_joyn2
  )
  expect_true(
    all(c("x", "y", "x & y") %in% jn_joyn$.joyn)
  )
  expect_true(
    all(
      c(jn_joyn$id) %in% c(y1$id, x2$id)
    )
  )

  # Second set of tables ----------------------
  jn_joyn <- merge(
    x = x2,
    y = y2,
    match_type = "1:1",
    by = "id"
  )

  jn_dt <- merge.data.table(
    x2,
    y2,
    match_type = "1:1",
    by = "id"
  )
  jn_dt <- jn_dt[order(jn_dt$id, na.last = T),]
  attr(
    jn_dt,
    "sorted"
  ) <- "id"

  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dt,
    ignore_attr = 'row.names'
  )


  jn <- merge(
    x4,
    y4,
    by = c("id1 = id2"),
    match_type = "m:m"
  )

  #merge.data.table(x4, y4, by = dplyr::join_by(id1 == id2), match_type = "m:m")
  jn_dt <- merge.data.table(
    x4,
    y4,
    by = dplyr::join_by(id1 == id2),
    match_type = "m:m"
  )
  attr(jn_dt, "sorted") <- "id1"
  expect_equal(
    jn |> fselect(-get(reportvar)),
    jn_dt,
    ignore_attr = '.internal.selfref'
  )

})


test_that("FULL JOIN - no id given", {

  jn1 <- merge(
    x2,
    y2
  )
  jn2 <- merge(
    x2,
    y2,
    by = c("id", "x")
  )
  expect_equal(jn1, jn2)

})


test_that("FULL JOIN - incorrectly specified arguments give errors", {

  expect_error(
    merge(
      x = x1,
      y = y1,
      match_type = "m:1",
      suffix = NULL
    )
  )

  expect_error(
    merge(
      x = x1,
      y = y1,
      match_type = "m:1",
      suffix = c("a", "b", "c")
    )
  )

  expect_error(
    merge(
      x = y1,
      y = x1,
      match_type = "1:m",
      multiple = "any"
    )
  )

  expect_error(
    merge(
      x = x1,
      y = y1,
      match_type = "m:1",
      unmatched = "error"
    )
  )


})


test_that("FULL JOIN - argument `keep` preserves keys in output", {

  jn <- merge(
    x = x1,
    y = y1,
    match_type = "m:1",
    keep = T,
    by = "id"
  )

  expect_true(
    "id.y" %in% names(jn)
  )
  expect_equal(
    jn |>
      fselect(id.y) |>
      na.omit() |>
      unique() |>
      reg_elem(),

    y1$id |>
      unique()
  )

})



test_that("FULL JOIN - update values works", {

  x2a <- x2
  x2a$x <- 1:5

  jn <- merge(
    x = x2a,
    y = y2,
    match_type = "1:1",
    update_values = TRUE,
    by = "id"
  )

  vupdated <- jn |>
    fsubset(get(reportvar) == "value updated") |>
    fselect(x.x) |>
    reg_elem()

  expect_true(
    all(vupdated %in% y2$x)
  )

  expect_equal(
    jn |>
      fsubset(get(reportvar) == "value updated") |>
      fnrow(),
    x2 |>
      fsubset(id %in% y2$id) |>
      fnrow()
  )

})


test_that("FULL JOIN - reportvar works", {

  jn <- merge(
    x1,
    y1,
    match_type = "m:1",
    by = "id",
    reportvar = "report"
  )
  expect_true(
    "report" %in% names(jn)
  )

})

test_that("FULL JOIN - NA matches", {

  jn <- merge(
    x5,
    y5,
    match_type = "m:m"
  )

  expect_equal(
    jn |>
      fsubset(is.na(id)) |>
      fnrow(),
    4
  )

})


# TEST INNER JOINS -------------------------------------------------------------

test_that("INNER JOIN - Conducts inner join", {

  # One way
  jn_joyn <- merge(
    x = x1,
    y = y1,
    match_type = "m:1",
    by = "id"
  )

  jn_dt <- merge.data.table(
    x1, y1,
    by = "id"
  )

  setorder(jn_dt, na.last = T)
  attr(
    jn_dt,
    "sorted"
  ) <- "id"

  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dt
  )
  expect_equal(
    jn_joyn,
    jn_joyn2
  )
  expect_true(
    all(c("x & y") %in% jn_joyn$.joyn)
  )
  expect_true(
    all(
      c(jn_joyn$id) %in% intersect(y1$id, x2$id)
    )
  )

  # Second set of tables ----------------------
  jn_joyn <- merge(
    x = x2,
    y = y2,
    match_type = "1:1",
    by = "id"
  )

  jn_dt <- merge.data.table(
    x2,
    y2,
    match_type = "1:1",
    by = "id"
  )
  jn_dt <- jn_dt[order(jn_dt$id, na.last = T),]
  attr(
    jn_dt,
    "sorted"
  ) <- "id"

  expect_equal(
    jn_joyn |> fselect(-get(reportvar)),
    jn_dt
  )


  jn <- merge(
    x4,
    y4,
    by = c("id1 = id2"),
    match_type = "m:m"
  )

  #merge.data.table(x4, y4, by = dplyr::join_by(id1 == id2), match_type = "m:m")
  jn_dt <- merge.data.table(
    x4,
    y4,
    by = dplyr::join_by(id1 == id2),
    match_type = "m:m"
  )
  attr(jn_dt, "sorted") <- "id1"
  expect_equal(
    jn |> fselect(-get(reportvar)),
    jn_dt,
    ignore_attr = '.internal.selfref'
  )

})


test_that("INNER JOIN - no id given", {

  jn1 <- merge(
    x2,
    y2
  )
  jn2 <- merge(
    x2,
    y2,
    by = c("id", "x")
  )
  expect_equal(jn1, jn2)

})


test_that("INNER JOIN - incorrectly specified arguments give errors", {

  expect_error(
    merge(
      x = x1,
      y = y1,
      match_type = "m:1",
      suffix = NULL
    )
  )

  expect_error(
    merge(
      x = x1,
      y = y1,
      match_type = "m:1",
      suffix = c("a", "b", "c")
    )
  )

  expect_error(
    merge(
      x = y1,
      y = x1,
      match_type = "1:m",
      multiple = "any"
    )
  )


})


test_that("INNER JOIN - argument `keep` preserves keys in output", {

  jn <- merge(
    x = x1,
    y = y1,
    match_type = "m:1",
    keep = T,
    by = "id"
  )

  expect_true(
    "id.y" %in% names(jn)
  )
  expect_equal(
    jn |>
      fselect(id.y) |>
      na.omit() |>
      unique() |>
      reg_elem(),
    y1 |>
      fsubset(id %in% x1$id) |>
      fselect(id) |>
      unique() |>
      reg_elem()
  )

})



test_that("INNER JOIN - update values works", {

  x2a <- x2
  x2a$x <- 1:5

  jn <- merge(
    x = x2a,
    y = y2,
    match_type = "1:1",
    update_values = TRUE,
    by = "id"
  )

  vupdated <- jn |>
    fsubset(get(reportvar) == "value updated") |>
    fselect(x.x) |>
    reg_elem()

  expect_true(
    all(vupdated %in% y2$x)
  )

  expect_equal(
    jn |>
      fsubset(get(reportvar) == "value updated") |>
      fnrow(),
    x2 |>
      fsubset(id %in% y2$id) |>
      fnrow()
  )



})


test_that("INNER JOIN - reportvar works", {

  jn <- merge(
    x1,
    y1,
    match_type = "m:1",
    by = "id",
    reportvar = "report"
  )
  expect_true(
    "report" %in% names(jn)
  )

})

test_that("INNER JOIN - NA matches", {


  jn <- merge(
    x5,
    y5,
    match_type = "m:m"
  )

  expect_equal(
    jn |>
      fsubset(is.na(id)) |>
      fnrow(),
    4
  )

})





