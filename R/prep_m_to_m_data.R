
prep_m_to_m_data <- function(x, y, by){

  # 1) re-order using `by` ----
  x <- x |>
    roworderv(
      by
    )
  y <- y |>
    roworderv(
      by
    )

  # 2) n_key ----
  x$n_key <- ave(
    x$key_column,
    x$key_column,
    FUN = seq_along
  )
  y$n_key <- ave(
    y$key_column,
    y$key_column,
    FUN = seq_along
  )

  # 3) Expansion ----
  # Compute the size of each group in both data frames
  x_size <- table(
    x[[by]]
  )
  y_size <- table(
    y[[by]]
  )
  # Expand x and y data frames
  x_expanded <- x[
    rep(
      seq_len(
        nrow(x)
      ),
      y_size[
        x[[by]]
      ]
    ),
  ]
  y_expanded <- y[
    rep(
      seq_len(
        nrow(y)
      ),
      x_size[
        y[[by]]
      ]
    ),
  ]

  # 4 and 5) Group `by` then create new count ----
  x_expanded$m_key <- ave(
    x_expanded$n_key,
    x_expanded$n_key,
    FUN = seq_along
  )
  y_expanded$m_key <- ave(
    y_expanded$n_key,
    y_expanded$n_key,
    FUN = seq_along
  )

  # 6 and 7) Create new m:m key ----
  x_expanded <- x_expanded |>
    ftransform(
      key_m_to_m = paste(
        n_original_rownumber,
        m_gp_rownumber,
        sep = "_"
      )
    )
  y_expanded <- y_expanded |>
    ftransform(
      key_m_to_m = paste(
        m_gp_rownumber,
        n_original_rownumber,
        sep = "_"
      )
    )

  # Clean final prepped data
  rownames(x_expanded) <- NULL
  rownames(y_expanded) <- NULL
  x_expanded <- x_expanded |>
    fselect(
      - c(
        n_original_rownumber,
        m_gp_rownumber
      )
    )
  y_expanded <- y_expanded |>
    fselect(
      - c(
        n_original_rownumber,
        m_gp_rownumber
      )
    )
  # Return list ----
  return(
    list(
      x = x_expanded,
      y = y_expanded
    )
  )

}



# Simulate and check
# Set seed for reproducibility
set.seed(124)

# Create dataset x
x <- data.frame(
  key_column = sample(LETTERS[1:2], 7, replace = TRUE),
  data_x = rnorm(7)
)

# Create dataset y
y <- data.frame(
  key_column = sample(LETTERS[1:2], 7, replace = TRUE),
  data_y = rnorm(7)
)

# Print the datasets
print(x)
print(y)

by = "key_column"

result <- prep_m_to_m_data(x, y, "key_column")

result
# Print the results
print(x)
print(y)
print(result$x)
print(result$y)

