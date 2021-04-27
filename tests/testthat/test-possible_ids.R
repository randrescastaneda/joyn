library(data.table)
# options(possible_ids.verbose = FALSE)

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
                            include = "x"))

})

test_that("exclude and include", {

  dd <- possible_ids(x3,
               exclude = "_numeric",
               include = "foo")
  expect_equal(c("V1", "V2"), names(dd))

})


test_that("get NULL when ducplicates", {

  expect_null(possible_ids(x1,
                           exclude = "_numeric",
                           include = "t"))

})


test_that("Exclude nothing", {

  expect_warning(possible_ids(x1,
                              exclude = "rer"))

})


test_that("Exclude type and variable", {

  xx4 <- copy(x4)

  xx4[, id2 := as.character(id2)]
  dd <- possible_ids(xx4,
               exclude = c("_character", "x"))

  expect_equal(c("id1", "t"), dd$V1)

})


test_that("Exclude more than one variable", {


  dd <- possible_ids(x4,
             exclude = c("id2", "x"))

  expect_equal(c("id1", "t"), dd$V1)

})


test_that("dplicated names", {
  xx4 <- copy(x4)
  setnames(xx4, "t", "x")

  expect_error(possible_ids(xx4))

})


