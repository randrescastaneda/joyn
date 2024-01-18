#' rename to syntactically valid names
#'
#' @param name character: name to be coerced to syntactically valid name
#' @inheritParams joyn
#'
#' @return valide character name
#' @export
#'
#' @examples
#' rename_to_valid("not valid")
rename_to_valid <- function(name, verbose = getOption("joyn.verbose")) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.character(name)) {
    cli::cli_abort("name {.var name} should be character")
  }
    nreportnames <- make.names(name)
    if (!identical(name,nreportnames) & isTRUE(verbose)) {

      cli::cli_alert_info("name {.field {name}} is an invalid variable name.
                          It will be changed to {.field {nreportnames}}",
                          wrap = TRUE)
    }
    return(nreportnames)
}



#' fix variable names in by argument
#'
#' @param by argument from merge
#' @param x left table
#' @param y right table
#'
#' @return list
#' @keywords internal
fix_by_vars <- function(by, x, y) {

  byexp <- grep("==?", by, value = TRUE)

  if (length(byexp) != 0) {

    xby <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\1", byexp))
    yby <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\3", byexp))
    newkeys <- paste0("keyby", 1:length(xby))

    setnames(x, xby, newkeys)
    setnames(y, yby, newkeys)

    by[grepl("==?", by)] <- newkeys

    return(list(by      = by,
                xby     = xby,
                yby     = yby,
                tempkey = newkeys)
    )

  } else {

    return(list(by      = by,
                xby     = NULL,
                yby     = NULL,
                tempkey = NULL)
    )

  }

}


#' Split matching type
#'
#' @inheritParams joyn
#'
#' @return character vector
#' @keywords internal
split_match_type <- function(match_type) {

  strsplit(match_type, ":", fixed = TRUE) |>
    unlist()

}


#' Is data frame balanced by group?
#'
#'
#'
#' @param df data frame
#' @param by  character: variables to check balance
#' @param return character: either "logic" or "table". If logic, returns `TRUE`
#'   or `FALSE` if data frame is balanced. If "table" returns the unbalanced
#'   observations - i.e. the combinations not found in input `df`
#'
#' @return logical if return == "logic", else returns data frame of unbalanced observations
#' @export
#'
#' @examples
#' x1 = data.frame(id = c(1L, 1L, 2L, 3L, NA_integer_),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = 11:15)
#' is_balanced(df = x1,
#'             by = c("id", "t"),
#'             return = "table") # returns combo of "id" and "t" not in df
#' is_balanced(df = x1,
#'             by = c("id", "t"),
#'             return = "logic") # returns TRUE or FALSE
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
                        collapse::funique()
                    })

  #_____________________________________________________
  # name of list elements ------------------------------
  names(lt_base) <- by

  #_____________________________________________________
  # elements as vectors, removing NAs ------------------
  lt_base <- lapply(lt_base,
                        function(df) {
                          df[[1]] |>
                            na_omit()
                        })

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

