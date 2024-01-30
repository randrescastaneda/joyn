# Test function fixing variable names in by argument

test_that("fix_by_vars -ouput", {
  res <- fix_by_vars(by ="id", x1, y1)

  class(res) |>
    expect_equal("list")

  names(res) |>
    expect_equal(c("by", "xby", "yby", "tempkey"))
})

