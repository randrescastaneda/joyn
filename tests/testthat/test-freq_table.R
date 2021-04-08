library(data.table)
test_that("correct frequencies", {

  b <- base::table(y4$id2)
  b <- as.numeric(b)

  j <- freq_table(y4, "id2")
  j <- j[ id2 != "total"
          ][, n]

  expect_equal(b, j)

})

test_that("correct totals", {

  tr <- nrow(y4)

  j <- freq_table(y4, "id2")
  j <- j[ id2 == "total"
          ][, n]

  expect_equal(tr, j)
})


