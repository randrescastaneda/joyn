# use collapse::join() backend
# keep all variables in x and y - give the suffix .x and .y
# always do a full join
# do a grid expansion if m:m join is requested
# standardise the naming conventions
# standardise teh column orders
# check timing
# add reporting column stating whether "x", "y", "x & y"


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




# Simulate and check
# Set seed for reproducibility
set.seed(124)

# Create dataset x
x <- data.frame(
  key_column = sample(LETTERS[1:3], 10, replace = TRUE),
  data_x = rnorm(10)
)

# Create dataset y
y <- data.frame(
  key_column = sample(LETTERS[1:2], 7, replace = TRUE),
  data_y = rnorm(7)
)


dt_merge_output <- merge.data.table(
  x = x,
  y = y,
  by = "key_column",
  allow.cartesian = T,
  suffix = c(".x", ".y"), # keep suffixes consistent for duplicated columns
  all = T
)

dt_merge_output
x
y
