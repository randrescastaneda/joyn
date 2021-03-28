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





