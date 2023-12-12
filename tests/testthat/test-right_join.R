withr::local_options(joyn.verbose = FALSE)
library(data.table)
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

################################################################################



test_that("Conducts right join", {

  # One way
  jn_joyn <- right_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id"
  )
  jn_joyn2 <- right_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id",
    unmatched = "drop"
  )

  jn_dplyr <- dplyr::right_join(
    x1, y1, by = "id", relationship = "many-to-one"
  )
  attr(
    jn_dplyr,
    "sorted"
  ) <- "id"

  expect_equal(
    jn_joyn |> fselect(-`.joyn`),
    jn_dplyr
  )
  expect_equal(
    jn_joyn,
    jn_joyn2
  )


  # Second set of tables ----------------------
  jn_joyn <- right_join(
    x = x2,
    y = y2,
    relationship = "one-to-one",
    by = "id"
  )

  jn_dplyr <- dplyr::right_join(
    x2,
    y2,
    relationship = "one-to-one",
    by = "id"
  )
  jn_dplyr <- jn_dplyr[order(jn_dplyr$id, na.last = T),]
  attr(
    jn_dplyr,
    "sorted"
  ) <- "id"

  expect_equal(
    jn_joyn |> fselect(-`.joyn`),
    jn_dplyr
  )


  jn <- right_join(
    x4,
    y4,
    by = c("id1 = id2"),
    relationship = "many-to-many"
  )

  #dplyr::right_join(x4, y4, by = dplyr::join_by(id1 == id2), relationship = "many-to-many")
  jn_dplyr <- dplyr::right_join(
    x4,
    y4,
    by = dplyr::join_by(id1 == id2),
    relationship = "many-to-many"
  )
  attr(jn_dplyr, "sorted") <- "id1"
  expect_equal(
    jn |> fselect(-`.joyn`),
    jn_dplyr
  )

})


test_that("no id given", {

  jn1 <- right_join(
    x2,
    y2
  )
  jn2 <- right_join(
    x2,
    y2,
    by = c("id", "x")
  )
  expect_equal(jn1, jn2)

})


test_that("incorrectly specified arguments give errors", {

  expect_error(
    right_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = NULL
    )
  )

  expect_error(
    right_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      suffix = c("a", "b", "c")
    )
  )

  expect_error(
    right_join(
      x = y1,
      y = x1,
      relationship = "one-to-many",
      multiple = "any"
    )
  )

  expect_error(
    right_join(
      x = x1,
      y = y1,
      relationship = "many-to-one",
      unmatched = "error"
    )
  )


})


test_that("argument `keep` preserves keys in output", {

  jn <- right_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    keep = T,
    by = "id"
  )

  expect_true(
    "id.x" %in% names(jn)
  )
  expect_equal(
    jn[, id.x] |> na.omit() |> unique(),
    x1[id %in% y1$id]$id |> unique()
  )

})



test_that("update values works", {

  x2a <- x2
  x2a$x <- 1:5

  jn <- right_join(
    x = x2a,
    y = y2,
    relationship = "one-to-one",
    update_values = TRUE,
    by = "id"
  )

  expect_true(
    all(jn[`.joyn` == "value updated",]$x.x %in% y2$x)
  )
  expect_equal(
    nrow(jn[`.joyn` == "value updated",]),
    nrow(x2[id %in% y2$id])
  )



})


test_that("reportvar works", {

  jn <- right_join(
    x1,
    y1,
    relationship = "many-to-one",
    by = "id",
    reportvar = "report"
  )
  expect_true(
    "report" %in% names(jn)
  )

})

test_that("NA matches", {


  jn <- right_join(
    x5,
    y5,
    relationship = "many-to-many"
  )

  expect_equal(
    jn[is.na(id), ] |> nrow(),
    4
  )


})







