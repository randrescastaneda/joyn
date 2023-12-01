
#' Internal function to add column to merged output table
#'
#' @param output_table output data object from joined x and y
#' @param x x (or 'left') data object used in the join to create the `output_table`
#' @param y x (or 'left') data object used in the join to create the `output_table`
#' @param by character vector: specify `by` argument for join,
#' must be common in all three tables
#'
#' @return data object of same class as `output_table` that has additional `_merge` columns -
#' `_merge` has value 1 if only in `x`, 2 if only in `y`, and 3 if in both
#' @export
#'
#' @examples
add_merge_report_column <- function(output_table, x, y, by){

  # Combine keys to check
  output_table <- dt_merge_output |>
    ftransform(
      check_key = paste(get(by), sep = "_")
    )
  check_key_x <- x |>
    ftransform(
      check_key_x = paste(get(by), sep = "_")
    ) |>
    fselect(
      check_key_x
    )
  check_key_y <- y |>
    ftransform(
      check_key_y = paste(get(by), sep = "_")
    ) |>
    fselect(
      check_key_y
    )

  # Logical vectors giving left, right
  left  <- output_table$check_key %in%
    check_key_x$check_key_x
  right <- output_table$check_key %in%
    check_key_y$check_key_y
  output_table$left <- left
  output_table$right <- right


  # Give only the report column
  output_table <- output_table |>
    ftransform(
      `_merge` = data.table::fcase(
        left == TRUE  & right == FALSE, 1,
        left == FALSE & right == TRUE,  2,
        left == TRUE  & right == TRUE,  3
      )
    )
  output_table <- output_table |>
    fselect(-c(left, right))

  # return
  return(output_table)


}
