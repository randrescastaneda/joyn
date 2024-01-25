
# Testing functions performing checks on x and y ####

withr::local_options(joyn.verbose = FALSE)

x1 = data.frame(
  id = c(1L, 1L, 2L, 3L, NA_integer_),
  t  = c(1L, 2L, 1L, 2L, NA_integer_),
  x  = 11:15
)

y1 = data.frame(id = 1:2,
                y  = c(11L, 15L))

x2 = data.frame(
  id = c(1, 1, 2, 3, NA),
  t  = c(1L, 2L, 1L, 2L, NA_integer_),
  x  = c(16, 12, NA, NA, 15)
)

y2 = data.frame(
  id = c(1, 2, 5, 6, 3),
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

# Test function checking x and y ---------------------------------------------------------------
test_that("check_xy works as expected", {

  # Inputs 
  check_xy(x = x1) |>
    expect_error("argument missing")
  
  check_xy(y = y1) |>
    expect_error("argument missing")

  # Output
  empty_x = data.frame()
  out_no_error <- check_xy(x1, y1)
  
  print(out_no_error) |>
    expect_equal(FALSE)
  
  # Note (RT): I might be missing why is check_xy always returning TRUE (?)
  
  out_error <- check_xy(empty_x, y) 
  
  print(out_error) |>
    expect_equal(TRUE)

})

# Test function checking reportvar ---------------------------------------------
test_that("check_reportvar works as expected", {
  
  # Inputs 
  check_reportvar(reportvar = NULL) |>
    expect_no_error()
  
  check_reportvar(reportvar = FALSE) |>
    expect_no_error()
  
  check_reportvar(reportvar = 2) |>
    expect_error()
  
  # Output

  check_reportvar(reportvar = NULL) |>
    expect_equal(check_reportvar(reportvar = FALSE))
  
  check_reportvar(reportvar = FALSE) |>
    expect_equal(NULL)
  
  # Valid name
  check_reportvar("id") |>
    expect_equal("id")
  
  # Invalid name converted to valid name 
  check_reportvar(" a and b") |>
    expect_message()
  

})

# Test function checking `by` input ---------------------------------------------------------------

test_that("check_by_vars function works as expected", {

  # Output
  res <- check_by_vars(by = 'id', x = x1, y = y1)

  class(res) |>
    expect_equal("list")
  
  names(res) |>
    expect_equal(c("by", "xby", "yby", "tempkey"))
  
  res$by |>
    expect_equal("by")
  
  # Check it throws an error when by is NULL 
  check_by_vars(by = NULL, x = x1, y = y1) |> 
    expect_error()

})

# Test function checking match type consistency -------------------------------------------------
test_that("check_match_type works as expected", {

  # Inputs 
  check_match_type(x1, y1, by= NULL) |>
    expect_error()
  
  check_match_type(x1, y1, by = "id", match_type = "invalid match_type") |>
    expect_error()

  # Outputs
  # Error if user choses "1" but actually "m" 
  check_match_type(x1, y1, by = "id", match_type = "1:1") |>
    expect_error()
  
  # If user choses "m" and it is actually "m"
  check_match_type(x1, y1, by = 'id', match_type = "m:1") |> 
    expect_equal(c("m", "1"))
  
  # Warning if user choses "m" but actually "1"
  # TODO
  class(check_match_type(x1, y1, by = 'id', match_type = "m:1")) |>
    expect_equal("character")
  
})

# Test function confirming if match_type_error ####
test_that("is_match_type_error works as expected", {
  #TODO
})

# Test function checking vars in Y kept in output table -----------------------------------------------------------------------------
test_that("y_vars_to_keep checks work", {
  ## var no available ---------
  y_vars_to_keep <- "f"
  by             <- "id"
  check_y_vars_to_keep(y_vars_to_keep, y1, by) |>
    expect_error()

  y_vars_to_keep <- c("F", "r")
  check_y_vars_to_keep(y_vars_to_keep, y1, by) |>
    expect_error()

  ## more than one value when no string --------
  y_vars_to_keep <- c(TRUE, TRUE)
  check_y_vars_to_keep(y_vars_to_keep, y1, by) |>
    expect_error()

  ## something besides character, false, or NULL ---------
  check_y_vars_to_keep(NA, y1, by) |>
    expect_error()

  # Output -------------
  check_y_vars_to_keep(NULL, y1, by) |>
    expect_null()

  check_y_vars_to_keep(FALSE, y1, by) |>
    expect_null()

  check_y_vars_to_keep("y", y1, by) |>
    expect_equal("y")

  check_y_vars_to_keep(c("id", "y"), y1, by) |>
    expect_equal("y")
  
  check_y_vars_to_keep(TRUE, y1, by) |>
    expect_equal("y")

})


test_that("check_new_y_vars checks work", {
  # errors -----------

  # output --------------

  ## no common names, return the same ---------
  check_new_y_vars(x = df1, by = "id1",
                   y_vars_to_keep = "salary") |>
    expect_equal("salary")

  ## add suffix -----------
  check_new_y_vars(x = df1, by = "id1",
                   y_vars_to_keep = c("id2", "salary")) |>
    expect_equal(c("id2.y", "salary"))
})

