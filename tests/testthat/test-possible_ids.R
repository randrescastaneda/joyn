
# PREPARATION ####
withr::local_options(joyn.verbose = FALSE)
library(data.table)
# options(possible_ids.verbose = FALSE)
x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = 11:15)

y1 = data.table(id = c(1,2, 4),
                y  = c(11L, 15L, 16))


x2 = data.table(id = c(1, 4, 2, 3, NA),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = c(16, 12, NA, NA, 15))


y2 = data.table(id = c(1, 2, 5, 6, 3),
                yd = c(1, 2, 5, 6, 3),
                y  = c(11L, 15L, 20L, 13L, 10L),
                x  = c(16:20))


y3 <- data.table(id = c("c","b", "c", "a"),
                 y  = c(11L, 15L, 18L, 20L))

x3 <- data.table(id=c("c","b", "d"),
                 v=8:10,
                 foo=c(4,2, 7))

x4 = data.table(id1 = c(1, 1, 2, 3, 3),
                id2 = c(1, 1, 2, 3, 4),
                t   = c(1L, 2L, 1L, 2L, NA_integer_),
                x   = c(16, 12, NA, NA, 15))


y4 = data.table(id  = c(1, 2, 5, 6, 3),
                id2 = c(1, 1, 2, 3, 4),
                y   = c(11L, 15L, 20L, 13L, 10L),
                x   = c(16:20))

# Auxiliary data: Big data table--------------------

# Set seed for reproducibility
set.seed(123)

# Number of rows and variables
n_rows <- 1e4        # 10,000 rows
n_vars <- 50         # Total variables

# Initialize an empty data.table
dt_large <- data.table(id = 1:n_rows)

## Manually create three variables that uniquely identify the data ####
dt_large[, unique_id1 := rep(1:10, each = 1000)]  # 1000 unique values repeated 100 times
dt_large[, unique_id2 := sample(letters, n_rows, replace = TRUE)]  # Random character variable
dt_large[, unique_id3 := sample(1:1000, n_rows, replace = TRUE)]   # Random integer

# Function to generate random data
generate_random_data <- function(n, type) {
  switch(type,
         "numeric_int" = sample(1:1e6, n, replace = TRUE),
         "numeric_double" = rnorm(n),
         "character" = replicate(n, paste0(sample(letters, 5, replace = TRUE), collapse = "")),
         "factor" = factor(sample(letters[1:10], n, replace = TRUE)),
         "logical" = sample(c(TRUE, FALSE), n, replace = TRUE),
         "date" = as.Date("2000-01-01") + sample(0:3650, n, replace = TRUE),
         "datetime" = as.POSIXct("2000-01-01") + sample(0:(3650*24*60*60), n, replace = TRUE)
  )
}

# Variable types and counts
var_types <- c("numeric_int", "numeric_double", "character", "factor", "logical", "date", "datetime")
vars_per_type <- c(10, 10, 10, 10, 5, 3, 2)  # Total should sum to 50

# Generate variables and add to the data.table
var_count <- 0
for (i in seq_along(var_types)) {
  type <- var_types[i]
  n_vars_type <- vars_per_type[i]
  for (j in 1:n_vars_type) {
    var_count <- var_count + 1
    var_name <- paste0(type, "_", j)
    dt_large[, (var_name) := generate_random_data(n_rows, type)]
  }
}

## Introduce duplicates in some columns that are NOT the unique identifiers ####
# For example, we can duplicate the first 100 rows in the "numeric_int_1" and "character_1" columns
# dt_large <- rbind(dt_large, dt_large[1:100, .(numeric_int_1, character_1)])

# Shuffle the data to avoid ordered data
dt_large <- dt_large[sample(.N)]



# dt_large[, id := .I]
dt <- copy(dt_large)


possible_ids(
  dt = dt_large,
  exclude_classes = c("numeric"),
  verbose = TRUE
)

possible_ids(
  dt = dt_large,
  exclude_classes = c("numeric"),
  exclude = "id",
  verbose = TRUE
)

uniq_vars <- grep("unique_id", names(dt_large), value = TRUE)
pids <- possible_ids(
  dt = dt_large,
  #exclude_classes = c("logical", "date", "datetime", "numeric"),
  exclude = "id",
  #vars = uniq_vars,
  verbose = TRUE,
  min_combination_size = 3,
  # max_combination_size = 3,
  max_processing_time = 240,
  get_all = TRUE
)

possible_ids(
  dt = dt_large,
  verbose = TRUE
)

## Remove the 'id' column to simulate data without a clear unique identifier ####
dt_large[, id := NULL]

possible_ids_list <- possible_ids(
  dt = dt_large,
  exclude_classes = c("logical", "date", "datetime"),  # Exclude some types for efficiency
  verbose = TRUE
)
possible_ids_list

possible_ids_list <- possible_ids(
  dt = dt_large,
  exclude_classes = c("logical", "date", "datetime", "numeric"),  # Exclude some types for efficiency
  max_processing_time = 120,
  verbose = TRUE
)
possible_ids_list



# TESTS ####
## Test create ids --------------------- ####

test_that("create_ids works as intended", {

  # with a single id
  res <- as.data.frame(create_ids(n_rows = 50, n_ids = 1))


})



## Test possible_ids ------------------- ####
test_that("convert to data.table", {
  xx1 <- as.data.frame(x1)
  expect_equal(possible_ids(x1), possible_ids(xx1))
})


test_that("store checked ids", {

  res <- possible_ids(x4,
                      get_all = TRUE)

  attributes(res)$checked_ids |>
    expect_setequal(names(x4))

  .joynenv$checked_ids |>
    expect_setequal(names(x4))

  # with get_all = FALSE
  res <- possible_ids(x4)

  attributes(res)$checked_ids |>
    expect_setequal(names(x4))

  .joynenv$checked_ids |>
    expect_setequal(names(x4))

  # with large dt -get_all FALSE
  res <- possible_ids(dt,
                      vars = paste0("numeric_int_", 1:4))

  attributes(res)$checked_ids |>
    expect_setequal(paste0("numeric_int_", 1:4))

  .joynenv$checked_ids |>
    expect_setequal(paste0("numeric_int_", 1:4))

  # with large dt -get_all TRUE
  res <- possible_ids(dt,
                      vars = paste0("numeric_int_", 1:4),
                      get_all = TRUE)

  attributes(res)$checked_ids |>
    expect_setequal(paste0("numeric_int_", 1:4))

  .joynenv$checked_ids |>
    expect_setequal(paste0("numeric_int_", 1:4))

})

test_that("error if not dataframe", {

  m1 <- as.matrix(x1)
  expect_error(possible_ids(m1))

})

test_that("vars provided by user", {

  # single var -raise error
  possible_ids(x4,
               vars = c("t")) |>
    expect_error()

  # one or more vars not included in dt
  possible_ids(x4,
               vars = c("id3", "id2")) |>
    expect_error()

  possible_ids(dt,
               vars = c("id", "id3", "id2")) |>
    expect_error()

  possible_ids(dt,
               vars = c("id", "numeric_int_1", "character_1"),
               verbose = TRUE) |>
    expect_no_error()

  ids_dt <- possible_ids(dt)

  vars <- c("id", "numeric_double_1", "numeric_double_2")

  use_ids_dt <- possible_ids(dt,
               vars = vars)

  all(sapply(use_ids_dt,
             function(x) { x %in% ids_dt })) |>
    expect_equal(TRUE)

  all(unlist(use_ids_dt) %in% vars) |>
    expect_equal(TRUE)


  # Check if the combination of unique_id1, unique_id2, and unique_id3 uniquely identifies rows
  vars <- c("id", "unique_id2", "unique_id3")

  res <- dt[, .N,
           by = vars][N > 1] |>
    nrow()

  # --if it does, possible_ids should return those vars

  if (res == 0) {  # if no duplicate rows
    possible_ids(dt,
                 vars = vars,
                 min_combination_size = 3) |>
      unlist() |>
      expect_setequal(vars)
  }

})

test_that("relationship include and vars", {

  possible_ids(x4,
               vars = c("id1", "id2"),
               include = c("t")) |>
    expect_no_error()

  possible_ids(dt,
               vars = c("id", "unique_id2"),
               include = c("id", "factor_1", "factor_2")) |>
    expect_no_error()

  possible_ids(x4,
               vars = NULL,
               include = c("t", "x")) |>
    expect_no_error()

  possible_ids(x4,
               vars = c("t", "x"),
               include = NULL) |>
    expect_no_error()

  # test checked vars are at least `vars` plus those in `include`
  res <- possible_ids(x4,
                      vars = c("id1", "id2", "t"),
                      include = "t")

  checked_ids <- attributes(res)$checked_ids

  checked_ids |>
    expect_setequal(c("id1", "id2", "t"))

  res <- possible_ids(dt,
                      vars = c("logical_1", "logical_2", "factor_1", "factor_2"),
                      include = "unique_id1")

  checked_ids <- attributes(res)$checked_ids

  checked_ids |>
    expect_setequal(c("logical_1", "logical_2", "factor_1", "factor_2", "unique_id1"))


})

test_that("relationship exclude and vars", {

  possible_ids(x4,
               vars = c("t", "x"),
               exclude_classes = "character") |>
    expect_message()

  possible_ids(x4,
               vars = c("id1", "x"),
               exclude = "x") |>
    expect_message()

  possible_ids(dt,
               vars = paste0("character_", 1:10),
               exclude = c("character_1", "character_2")) |>
    expect_message()
  })

# test_that("inconsistent use of `include`", {
#
#   #  expect_warning(possible_ids(x1,
#   #                              include = "x"))
#   #
#   # possible_ids(x1,
#   #              include = c("id", "x")) |>
#   #   expect_no_error()
#
# })

test_that("exclude and include", {

  dd <- possible_ids(x3,
               exclude_classes =  c("numeric", "integer"),
               include = "foo")

  expect_equal(unlist(dd),
               c("id", "foo"))

  ## Test combination between include class and exclude vars ####

  res <- possible_ids(dt,
                               get_all = TRUE,
                               include_classes     = c("integer"),
                               exclude             = paste0("numeric_int_", 1:5))

  checked_vars <- attributes(res)$checked_ids

  any(
    paste0("numeric_int_", 1:5) %in% checked_vars
    ) |>
    expect_equal(FALSE)

  all(
    paste0("numeric_int_", 6:10) %in% checked_vars
  ) |>
    expect_equal(TRUE)

  ## Test combination between include vars and exclude class ####
  res <- possible_ids(dt,
                      include     = c("numeric_double_1",
                                      "numeric_double_2"),
                      exclude_classes = "numeric")

  checked_vars <- attributes(res)$checked_ids

  all(
    paste0("numeric_double_", 1:2) %in% checked_vars
  ) |>
    expect_equal(TRUE)

  any(
    paste0("numeric_double_", 3:10) %in% checked_vars
  ) |>
    expect_equal(FALSE)

  res_ids <- possible_ids(x2,
                               include = "x",
                               exclude_classes = "numeric")

  res_ids |>
    unlist() |>
    expect_equal(c("t", "x"))

  res_ids <- possible_ids(x3,
                          include = "id",
                          exclude_classes = "character")

  res_ids |>
    unlist() |>
    expect_setequal(c("foo", "id", "v"))

  # alert if include and exclude same class ####
  possible_ids(dt,
               include_classes = "numeric",
               exclude_classes = "numeric") |>
    expect_message()

  # alert if include and exclude same vars ####
  possible_ids(dt,
               include = c("id", "unique_id1"),
               exclude = c("id", "unique_id1")) |>
    expect_message()

  res <- possible_ids(dt,
                      exclude_classes = c("integer"),
                      include = c("numeric_int_1"))

  attributes(res)$checked_ids |>
    expect_setequal(setdiff(names(dt), c(paste0("numeric_int_", 2:10),
                                         "id",
                                         "unique_id1", "unique_id3")))

})


# test_that("get length 0", {
#
#   expect_length(possible_ids(x1,
#                            exclude_classes = c("numeric", "integer"),
#                            include = "t"), 0)
#
# })

test_that("get length 0 -error", {

  expect_error(possible_ids(x1,
                           exclude_classes = c("numeric", "integer"),
                           include = "t"))

})

test_that("get all works", {

  # no error
  possible_ids(x4,
               get_all = TRUE) |>
    expect_no_error()

  # get all with user selected vars
  possible_ids(x4,
               vars = c("id1", "t"),
               get_all = TRUE) |>
    expect_no_error()

  # get all with max number of combinations
  possible_ids(x4,
               max_combination_size = 3,
               get_all = TRUE) |>
    expect_no_error()

  # check get all combs
  possible_ids(x3,
               get_all = TRUE) |>
    unlist() |>
    expect_setequal(c("id", "v", "foo"))



})


test_that("Max combination size", {

  res <- possible_ids(dt,
               vars = c( "unique_id1", "unique_id2", "unique_id3",
                        "character_1", "character_2", "character_3", "character_4"),
               max_combination_size = 5)


  sapply(res, function(sublist) {
    length(sublist) <= 3}) |>
    all() |>
    expect_true()

  res <- possible_ids(x1,
                      get_all = TRUE,
                      max_combination_size = 2)

  sapply(res, function(sublist) {
    length(sublist) <= 2}) |>
    all() |>
    expect_true()

})

test_that("Min combination size", {

  res <- possible_ids(x4,
               min_combination_size = 1,
               get_all = FALSE) |>
    unlist()

  expect_true(
    length(res) >= 1)

  res <- possible_ids(dt,
               min_combination_size = 3,
               get_all = FALSE) |>
    unlist()

  expect_true(length(res) >= 3)


  possible_ids(x4,
               #min_combination_size = 1,
               max_combination_size = 1) |>
    expect_error()


  possible_ids(x4,
               min_combination_size = 3,
               max_combination_size = 2) |>
    expect_error()

})


test_that("Exclude nothing", {
  p1 <- possible_ids(x1)
  p2 <- possible_ids(x1, exclude = "rer")

  expect_equal(p1, p2)

})


test_that("Exclude type and variable", {

  xx4 <- copy(x4)

  xx4[, id2 := as.character(id2)]
  dd <- possible_ids(xx4,
               exclude_classes = c("character"),
               exclude = "x")

  expect_equal(c("id1", "t"), unlist(dd))

})


test_that("Exclude more than one variable", {


  dd <- possible_ids(x4,
             exclude = c("id2", "x"))

  expect_equal(c("id1", "t"), unlist(dd))

})


test_that("duplicated names", {
  xx4 <- copy(x4)
  setnames(xx4, "t", "x")

  expect_error(possible_ids(xx4))

})

# Auxiliary data: Big data table--------------------

# Set seed for reproducibility
set.seed(123)

# Number of rows and variables
n_rows <- 1e4        # 10,000 rows
n_vars <- 50         # Total variables

# Initialize an empty data.table
dt_large <- data.table(id = 1:n_rows)

## Manually create three variables that uniquely identify the data ####
dt_large[, unique_id1 := rep(1:10, each = 1000)]  # 1000 unique values repeated 100 times
dt_large[, unique_id2 := sample(letters, n_rows, replace = TRUE)]  # Random character variable
dt_large[, unique_id3 := sample(1:1000, n_rows, replace = TRUE)]   # Random integer

# Function to generate random data
generate_random_data <- function(n, type) {
  switch(type,
         "numeric_int" = sample(1:1e6, n, replace = TRUE),
         "numeric_double" = rnorm(n),
         "character" = replicate(n, paste0(sample(letters, 5, replace = TRUE), collapse = "")),
         "factor" = factor(sample(letters[1:10], n, replace = TRUE)),
         "logical" = sample(c(TRUE, FALSE), n, replace = TRUE),
         "date" = as.Date("2000-01-01") + sample(0:3650, n, replace = TRUE),
         "datetime" = as.POSIXct("2000-01-01") + sample(0:(3650*24*60*60), n, replace = TRUE)
  )
}

# Variable types and counts
var_types <- c("numeric_int", "numeric_double", "character", "factor", "logical", "date", "datetime")
vars_per_type <- c(10, 10, 10, 10, 5, 3, 2)  # Total should sum to 50

# Generate variables and add to the data.table
var_count <- 0
for (i in seq_along(var_types)) {
  type <- var_types[i]
  n_vars_type <- vars_per_type[i]
  for (j in 1:n_vars_type) {
    var_count <- var_count + 1
    var_name <- paste0(type, "_", j)
    dt_large[, (var_name) := generate_random_data(n_rows, type)]
  }
}

## Introduce duplicates in some columns that are NOT the unique identifiers ####
# For example, we can duplicate the first 100 rows in the "numeric_int_1" and "character_1" columns
# dt_large <- rbind(dt_large, dt_large[1:100, .(numeric_int_1, character_1)])

# Shuffle the data to avoid ordered data
dt_large <- dt_large[sample(.N)]



# dt_large[, id := .I]
dt <- copy(dt_large)


possible_ids(
  dt = dt_large,
  exclude_classes = c("numeric"),
  verbose = TRUE
)

possible_ids(
  dt = dt_large,
  exclude_classes = c("numeric"),
  exclude = "id",
  verbose = TRUE
)

uniq_vars <- grep("unique_id", names(dt_large), value = TRUE)
pids <- possible_ids(
  dt = dt_large,
  #exclude_classes = c("logical", "date", "datetime", "numeric"),
  exclude = "id",
  #vars = uniq_vars,
  verbose = TRUE,
  min_combination_size = 3,
  # max_combination_size = 3,
  max_processing_time = 240,
  get_all = TRUE
)

possible_ids(
  dt = dt_large,
  verbose = TRUE
)

## Remove the 'id' column to simulate data without a clear unique identifier ####
dt_large[, id := NULL]

possible_ids_list <- possible_ids(
  dt = dt_large,
  exclude_classes = c("logical", "date", "datetime"),  # Exclude some types for efficiency
  verbose = TRUE
)
possible_ids_list

possible_ids_list <- possible_ids(
  dt = dt_large,
  exclude_classes = c("logical", "date", "datetime", "numeric"),  # Exclude some types for efficiency
  max_processing_time = 120,
  verbose = TRUE
)
possible_ids_list
