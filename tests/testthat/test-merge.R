library(data.table)
test_that("slect by vars when no specified", {
  expect_equal(merge(x1, y1, verbose = FALSE),
               merge(x1, y1, verbose = FALSE, by = "id")
               )

})


test_that("Erro if no common variables", {
  xf <- copy(x1)
  xf[, id := NULL]
  expect_error(merge(xf, y1))
})

test_that("m:m and 1:1 gives the same if data is correct", {

  expect_equal(merge(x2, y2, by = "id", update_values = TRUE, join_type = "1:1"),
               merge(x2, y2, by = "id", update_values = TRUE))

  expect_equal(merge(x2, y2, by = "id", updateNA = TRUE, join_type = "1:1"),
               merge(x2, y2, by = "id", updateNA = TRUE))

  expect_equal(merge(x2, y2, by = "id", join_type = "1:1"),
               merge(x2, y2, by = "id", ))

})








