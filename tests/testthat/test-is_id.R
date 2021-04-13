library(data.table)
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

