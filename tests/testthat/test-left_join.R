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


################################################################################


test_that("Conducts left join", {

  # One way
  jn_joyn <- left_join(
    x = x1,
    y = y1,
    relationship = "many-to-one",
    by = "id"
  )

  jn_dplyr <- data.table(id = c(1, 1, 2, 3, NA),
                         t = c(1, 2, 1, 2, NA),
                         x = c(11, 12, 13, 14, 15),
                         y = c(11, 11, 15, NA, NA))
  attr(
    jn_dplyr,
    "sorted"
  ) <- "id"

  expect_equal(
    jn_joyn |> fselect(-`.joyn`),
    jn_dplyr
  )

  # Second set of tables ----------------------
  jn_joyn <- left_join(
    x = x2,
    y = y2,
    relationship = "one-to-one",
    by = "id"
  )

  jn_dplyr <- data.table(id  = c(1, 4, 2, 3, NA),
                         t   = c(1, 2, 1, 2, NA),
                         x.x = c(16, 12, NA, NA, 15),
                         yd  = c(1, NA, 2, 3, NA),
                         y   = c(11, NA, 15, 10, NA),
                         x.y = c(16, NA, 17, 20, NA))
  jn_dplyr <- jn_dplyr[order(jn_dplyr$id, na.last = T),]
  jn_dplyr[
    ,
    x := x.x
  ]
  jn_dplyr$x.x <- NULL
  jn_dplyr <- jn_dplyr |> fselect(names(jn_joyn |> fselect(-`.joyn`)))
  attr(
    jn_dplyr,
    "sorted"
  ) <- "id"

  expect_equal(
    jn_joyn |> fselect(-`.joyn`),
    jn_dplyr
  )


})


