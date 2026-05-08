.joynenv <- new.env(parent = emptyenv())
rlang::env_poke(.joynenv, "msg_type_choices", c("info", "note", "warn", "timing", "err"))
set_collapse(mask = "%in%")

# Package-level constant: allowed classes for join key variables
#' @keywords internal
#' @name .joyn_allowed_classes
.joyn_allowed_classes <- c(
  "character",
  "integer",
  "numeric",
  "factor",
  "logical",
  "Date",
  "POSIXct"
  # "fs_path" passes via "character" (fs_path inherits character) — no entry needed
  # "POSIXlt" excluded intentionally: data.table does not support it.
  # POSIXct objects have class c("POSIXct","POSIXt"), so "POSIXct" alone suffices.
)
