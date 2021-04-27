library(data.table)
options(possible_ids.verbose = FALSE)

test_that("convert to data.table", {
  xx1 <- as.data.frame(x1)
  expect_equal(possible_ids(x1), possible_ids(xx1))
})

test_that("error if not dataframe", {

  m1 <- as.matrix(x1)
  expect_error(possible_ids(m1))

})

test_that("inconsistent user of `include`", {

  expect_warning(possible_ids(x1,
                            include = "x",
                            verbose = TRUE))

})

test_that("exclude and include", {

  dd <- possible_ids(x3,
               exclude = "_numeric",
               include = "foo")
  expect_equal(c("V1", "V2"), names(dd))

})


test_that("get when ducplicates", {

  expect_null(possible_ids(x1,
                           exclude = "_numeric",
                           include = "t"))

})



