# Test function fixing variable names in by argument

x1 = data.frame(
  id = c(1L, 1L, 2L, 3L, NA_integer_),
  t  = c(1L, 2L, 1L, 2L, NA_integer_),
  x  = 11:15
)

y1 = data.table(id = c(1,2, 4),
                y  = c(11L, 15L, 16))

df1 <- data.frame(
  id = c(1, 1, 2, 3),
  id1 = c("a", "b", "b", "c"),
  name = c("John", "Jane", "Bob", "Carl"),
  age = c(35, 28, 42, 50)
)

df2 <- data.frame(
  id2 = c(1, 2, 3, 3),
  id3 = c("a", "b", "c", "e"),
  salary = c(60000, 55000, 70000, 80000),
  dept = c("IT", "Marketing", "Sales", "IT")
)

# Checks

test_that("fix_by_vars works as expected", {

  res_1 <- fix_by_vars(by ="id",
                       x  = x1,
                       y  = y1)

  class(res_1) |>
    expect_equal("list")

  names(res_1) |>
    expect_equal(c("by",
                   "xby",
                   "yby",
                   "tempkey"))
  # Output values
  res_1$by |>
      expect_equal("id")

  res_1$xby |>
      expect_equal(NULL)

  res_1$yby |>
      expect_equal(NULL)

  res_1$tempkey |>
      expect_equal(NULL)

  res_2 <- fix_by_vars(by = "id == id2",
                       x  = df1,
                       y  = df2)
  res_2$by |>
      expect_equal("keyby1")

  res_2$xby |>
    expect_equal("id")

  res_2$yby |>
    expect_equal("id2")

  res_2$tempkey |>
    expect_equal("keyby1")

})

