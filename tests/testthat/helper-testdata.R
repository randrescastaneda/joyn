#' Helper to create test datasets for check_xy and related tests
#' @keywords internal
make_test_data <- function() {
  x1 <- data.frame(
    id = c(1L, 1L, 2L, 3L, NA_integer_),
    t  = c(1L, 2L, 1L, 2L, NA_integer_),
    x  = 11:15
  )

  x1_duplicates <- data.frame(
    id = c(1L, 1L, 2L, 3L, NA_integer_),
    x = c(1L, 2L, 1L, 2L, NA_integer_),
    x = 11:15,
    check.names = FALSE
  )

  y1 <- data.table(
    id = c(1, 2, 4),
    y  = c(11L, 15L, 16)
  )

  x2 <- data.frame(
    id = c(1, 1, 2, 3, NA),
    t  = c(1L, 2L, 1L, 2L, NA_integer_),
    x  = c(16, 12, NA, NA, 15)
  )

  x3 <- data.frame(
    id = c(1, 2, 3, 4, 5),
    t  = c(1L, 2L, 5L, 6L, NA_integer_),
    x  = c(16, 12, NA, NA, 15)
  )

  y2 <- data.frame(
    id = c(1, 2, 5, 6, 3),
    yd = c(1, 2, 5, 6, 3),
    y  = c(11L, 15L, 20L, 13L, 10L),
    x  = c(16:20)
  )

  y3 <- data.frame(
    id = c(1, 2, 2, 6, 3),
    yd = c(1, 2, 5, 6, 3),
    y  = c(11L, 15L, 20L, 13L, 10L),
    x  = c(16:20)
  )

  df1 <- data.frame(
    id1 = c(1, 1, 2, 3),
    id2 = c("a", "b", "b", "c"),
    name = c("John", "Jane", "Bob", "Carl"),
    age = c(35, 28, 42, 50)
  )

  df2 <- data.frame(
    id1 = c(1, 2, 3, 3),
    id2 = c("a", "b", "c", "e"),
    salary = c(60000, 55000, 70000, 80000),
    dept = c("IT", "Marketing", "Sales", "IT")
  )

  list(
    x1 = x1, x1_duplicates = x1_duplicates, x2 = x2, x3 = x3,
    y1 = y1, y2 = y2, y3 = y3, df1 = df1, df2 = df2
  )
}
