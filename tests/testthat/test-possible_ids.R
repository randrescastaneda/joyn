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


test_that("get NULL when duplicates", {

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


test_that("duplicated names", {
  xx4 <- copy(x4)
  setnames(xx4, "t", "x")

  expect_error(possible_ids(xx4))

})



library(lubridate)   # For date functions

# Set seed for reproducibility
set.seed(123)

# Number of rows and variables
n_rows <- 1e5        # 100,000 rows
n_vars <- 50         # Total variables

# Initialize an empty data.table
dt_large <- data.table(n = 1:n_rows)

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
vars_per_type <- c(10, 10, 10, 10, 5, 3, 2)  # sum to 50

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

# Introduce duplicates intentionally
# Duplicate the first 100 rows
dt_large <- rowbind(dt_large, dt_large[1:100, ])

# Shuffle the data
dt_large <- dt_large[sample(.N)]
dt_large[, id := .I]


possible_ids(
  dt = dt_large,
  verbose = TRUE
)

# Remove the 'id' column to simulate data without a clear unique identifier
dt_large[, id := NULL]

possible_ids_list <- possible_ids(
  dt = dt_large,
  exclude_types = c("logical", "date", "datetime"),  # Exclude some types for efficiency
  verbose = TRUE
)
possible_ids_list


# Display the structure of the data.table
str(dt_large)






