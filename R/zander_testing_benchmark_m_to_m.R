# compare speed of different grid expansion approaches to the m:m join



### 1) standard ----------------------------------------------------------------
joyn_workhorse <- function(x, y, by, match_type = NULL) {

  start_time <- Sys.time()

  if (!is.null(match_type) && match_type == "m:m") {

    m_to_m_data <- prep_m_to_m_data(

    )

    x
    y <-


      results <- collapse::join()


  } else {
    # Perform the full join using original data frames
    result <- collapse::join(x, y, how = "full",
                             suffix = c(".x", ".y"),
                             keep.col.order = TRUE,
                             column = list("_merge", c("1", "2", "3")))
  }

  # Calculate the time taken
  end_time <- Sys.time()
  time_taken <- end_time - start_time

  # Return as a list with the result and time taken
  list(result = result, time_taken = time_taken)
}

### 2) Grid Expansion ----------------------------------------------------------
joyn_workhorse_expand_grid <- function(x, y, match_type = NULL) {
  start_time <- Sys.time()

  if (!is.null(match_type) && match_type == "m:m") {
    keys <- unique(c(x$id, y$id))
    expansion <- expand.grid(id = keys, key_x = 1:nrow(x), key_y = 1:nrow(y))

    x_expanded <- collapse::join(expansion, x, on = "key_x", how = "left",
                                 suffix = c("", ".x"))
    y_expanded <- collapse::join(expansion, y, on = "key_y", how = "left",
                                 suffix = c("", ".y"))

    result <- collapse::join(x_expanded, y_expanded, on = "id", how = "full",
                             suffix = c(".x", ".y"), keep.col.order = TRUE,
                             column = list("_merge", c("1", "2", "3")))
  } else {
    # Perform the full join using original data frames
    result <- collapse::join(x, y, how = "full",
                             suffix = c(".x", ".y"),
                             keep.col.order = TRUE,
                             column = list("_merge", c("1", "2", "3")))
  }

  # Calculate the time taken
  end_time <- Sys.time()
  time_taken <- end_time - start_time

  # Return as a list with the result and time taken
  list(result = result, time_taken = time_taken)
}

### 3) Tidyr approach ----------------------------------------------------------
library(tidyr)

joyn_workhorse_tidyr <- function(x, y, match_type = NULL) {
  start_time <- Sys.time()

 if (!is.null(match_type) && match_type == "m:m") {
    keys <- unique(c(x$id, y$id))

    x_expanded <- crossing(id = keys, key_x = 1:nrow(x))
    x_expanded <- collapse::join(x_expanded, x, on = "key_x", how = "left",
                                 suffix = c("", ".x"))

    y_expanded <- crossing(id = keys, key_y = 1:nrow(y))
    y_expanded <- collapse::join(y_expanded, y, on = "key_y", how = "left",
                                 suffix = c("", ".y"))

    result <- collapse::join(x_expanded, y_expanded, on = "id", how = "full",
                             suffix = c(".x", ".y"), keep.col.order = TRUE,
                             column = list("_merge", c("1", "2", "3")))
  } else {
    # Perform the full join using original data frames
    result <- collapse::join(x, y, how = "full",
                             suffix = c(".x", ".y"),
                             keep.col.order = TRUE,
                             column = list("_merge", c("1", "2", "3")))
  }

  # Calculate the time taken
  end_time <- Sys.time()
  time_taken <- end_time - start_time

  # Return as a list with the result and time taken
  list(result = result, time_taken = time_taken)
}

### 3) Tidyr approach ----------------------------------------------------------

library(microbenchmark)

# Simulate x and y data tables
set.seed(124)
x <- data.frame(
  id = sample(
    LETTERS[1:2],
    8,
    replace = TRUE
  ),
  val = rnorm(8)
)
x <- x |>
  mutate(key = row_number())
y <- data.frame(
  id = sample(
    LETTERS[1:2],
    8,
    replace = TRUE
  ),
  val = rnorm(8)
)
y <- y |>
  mutate(key = row_number())

# Benchmark the functions
benchmark_results <- microbenchmark(
  original    = joyn_workhorse(
    x,
    y,
    "m:m"
  ),
  expand_grid = joyn_workhorse_expand_grid(
    x,
    y,
    "m:m"
  ),
  tidyr       = joyn_workhorse_tidyr(
    x,
    y,
    "m:m"
  ),
  times       = 10
)

print(benchmark_results)


