withr::local_options(joyn.verbose = FALSE)
library(data.table)
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

x3 <- data.table(id=c("c","b", "d"),
                 v=8:10,
                 foo=c(4,2, 7))

x4 = data.table(id1 = c(1, 1, 2, 3, 3),
                id2 = c(1, 1, 2, 3, 4),
                t   = c(1L, 2L, 1L, 2L, NA_integer_),
                x   = c(16, 12, NA, NA, 15))


y4 = data.table(id  = c(1, 2, 5, 6, 3),
                id2 = c(1, 1, 2, 3, 4),
                y   = c(11L, 15L, 20L, 13L, 10L),
                x   = c(16:20))

test_that("identifies no ids", {

  y <- data.table(
      id = c("c", "b", "c", "a"),
      y = c(11L, 15L, 18L, 20L)
    )

  expect_false(is_id(y, by = "id"))


  z <- data.table(
    id = c(1:1e7),
    a  = "z"
  )

  expect_true(is_id(z, by = "id"))


  w <- data.table(
    id = c(1, 1:1e7),
    a  = "w"
  )

  expect_false(is_id(w, by = "id"))


})

test_that("returns correct report table", {

  y <- data.table(
      id = c("c", "b", "c", "a"),
      y = c(11L, 15L, 18L, 20L)
    )
  j <- is_id(y, by = "id", return_report = TRUE)

  r <- data.table(id = c("c", "b", "a"),
                  copies = c(2L, 1L, 1L))

  expect_equal(j, r)

})


test_that("convert to data.table when dataframe", {

  yy3 <- as.data.frame(y3)

  expect_false(is_id(yy3, "id"))

})

