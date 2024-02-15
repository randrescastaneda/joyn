#' rename to syntactically valid names
#'
#' @param name character: name to be coerced to syntactically valid name
#' @inheritParams joyn
#'
#' @return valid character name
#' @export
#'
#' @examples
#' joyn:::rename_to_valid("x y")
rename_to_valid <- function(name, verbose = getOption("joyn.verbose")) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.character(name)) {
    cli::cli_abort("name {.var name} should be character")
  }
  nreportnames <- make.names(name)
  if (!identical(name, nreportnames) & isTRUE(verbose)) {

    cli::cli_alert_info("name {.field {name}} is an invalid variable name.
                        It will be changed to {.field {nreportnames}}",
                        wrap = TRUE)
  }
  return(nreportnames)
}

#' Split matching type
#'
#' Split matching type (one of `"1:1", "m:1", "1:m", "m:m"`) into its two components
#'
#' @inheritParams joyn
#'
#' @return character vector
#' @keywords internal
split_match_type <- function(match_type) {

  match_types <- c("1:1", "m:1", "1:m", "m:m")

  if (!match_type %in% match_types) {
    cli::cli_abort("invalid match type")
  }

  strsplit(match_type, ":", fixed = TRUE) |>
    unlist()

}


#' Is data frame balanced by group?
#'
#' Check if the data frame is balanced by group of columns, i.e., if it contains every combination of the elements in the specified variables
#'
#' @param df data frame
#' @param by  character: variables used to check if `df` is balanced
#' @param return character: either "logic" or "table". If "logic", returns `TRUE`
#'   or `FALSE` depending on whether data frame is balanced. If "table" returns the unbalanced
#'   observations - i.e. the combinations of elements in specified variables not found in input `df`
#'
#' @return logical, if return == "logic", else returns data frame of unbalanced observations
#' @export
#'
#' @examples
#' x1 = data.frame(id = c(1L, 1L, 2L, 3L, NA_integer_),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = 11:15)
#' is_balanced(df = x1,
#'             by = c("id", "t"),
#'             return = "table") # returns combination of elements in "id" and "t" not present in df
#' is_balanced(df = x1,
#'             by = c("id", "t"),
#'             return = "logic") # FALSE
is_balanced <- function(df,
                        by,
                        return = c("logic", "table")) {

  #_____________________________________________________
  # Arguments-------------------------------------------
  return <- match.arg(return)
  if (!is.character(by)) {
    cli::cli_abort("Argument `by` must be character vector")
  }
  if (!all(by %in% names(df))) {
    cli::cli_abort("`by` must give column names of `df`")

  }
  #_____________________________________________________
  # unique values of "by" ------------------------------
  lt_base <- lapply(by,
                    function(y){
                      df |>
                        get_vars(y) |>
                        funique() |>
                        na_omit() |>
                        reg_elem()
                    })

  #_____________________________________________________
  # name of list elements ------------------------------
  names(lt_base) <- by

  #_____________________________________________________
  # expand grid ----------------------------------------
  df_balanced <-
    expand.grid(lt_base,
                stringsAsFactors = FALSE) |>
    collapse::qDF()

  #_____________________________________________________
  #
  #unmatch  <- dt_balanced[!dt,  on = by]
  unmatch   <- collapse::join(
    x       = df_balanced,
    y       = df,
    how     = "anti",
    verbose = FALSE
  )
  balanced <- c(nrow(unmatch) == 0)

  if (return == "logic") {
    return(balanced)
  } else {
    setattr(unmatch, "is_balanced", balanced)
    return(unmatch)
  }

}

