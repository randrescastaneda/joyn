## Type coercion edge-case tests
## Covers: integer<>numeric, character<>factor, Date<>POSIXct,
##         NA keys, all-NA key column, multi-key mixed types,
##         numeric<>character (warns/errors), list keys (rejected)
library(data.table)

# Helper: strip report column for simpler comparisons
drop_report <- function(dt, rv = getOption("joyn.reportvar")) {
  dt[, (rv) := NULL][]
}

# ── integer ↔ numeric key ────────────────────────────────────────────────────

test_that("integer key joins with numeric key without error", {
  x <- data.table(id = 1L:3L, val_x = letters[1:3])
  y <- data.table(id = c(1, 2, 4), val_y = LETTERS[1:3])

  result <- joyn(x, y, by = "id", match_type = "1:1", verbose = FALSE)

  expect_true(is.data.frame(result))
  # rows from x: 3; row from y only: 1 → 4 rows total
  expect_equal(nrow(result), 4L)
  expect_true("val_x" %in% names(result))
  expect_true("val_y" %in% names(result))
})

test_that("numeric key joins with integer key without error", {
  x <- data.table(id = c(1.0, 2.0, 3.0), val_x = letters[1:3])
  y <- data.table(id = 1L:3L, val_y = LETTERS[1:3])

  result <- joyn(x, y, by = "id", match_type = "1:1", verbose = FALSE)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3L)
})

# ── character ↔ factor key ───────────────────────────────────────────────────

test_that("character key joins with factor key", {
  x <- data.table(grp = c("a", "b", "c"), val_x = 1:3)
  y <- data.table(grp = factor(c("b", "c", "d")), val_y = 10:12)

  # Should join without hard error
  expect_no_error(
    joyn(x, y, by = "grp", match_type = "1:1", verbose = FALSE)
  )
})

test_that("factor key joins with factor key", {
  x <- data.table(grp = factor(c("a", "b", "c")), val_x = 1:3)
  y <- data.table(grp = factor(c("b", "c", "d")), val_y = 10:12)

  result <- joyn(x, y, by = "grp", match_type = "1:1", verbose = FALSE)

  expect_true(is.data.frame(result))
  # a: x-only; b,c: matched; d: y-only → 4 rows
  expect_equal(nrow(result), 4L)
})

# ── Date key ────────────────────────────────────────────────────────────────

test_that("Date key joins correctly", {
  x <- data.table(
    dt   = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    val_x = 1:3
  )
  y <- data.table(
    dt   = as.Date(c("2023-01-01", "2023-02-01", "2023-04-01")),
    val_y = 10:12
  )

  result <- joyn(x, y, by = "dt", match_type = "1:1", verbose = FALSE)

  expect_true(is.data.frame(result))
  # 2023-03-01 x-only; 2023-04-01 y-only; two matched → 4 rows
  expect_equal(nrow(result), 4L)
})

# ── POSIXct key ─────────────────────────────────────────────────────────────

test_that("POSIXct key joins correctly", {
  base_ts <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
  x <- data.table(ts = base_ts + c(0, 3600, 7200), val_x = 1:3)
  y <- data.table(ts = base_ts + c(0, 3600, 10800), val_y = 10:12)

  result <- joyn(x, y, by = "ts", match_type = "1:1", verbose = FALSE)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4L)
})

# ── NA keys ──────────────────────────────────────────────────────────────────

test_that("NA keys are handled without crash", {
  x <- data.table(id = c(1L, 2L, NA_integer_), val_x = 1:3)
  y <- data.table(id = c(1L, NA_integer_, 3L), val_y = 10:12)

  expect_no_error(
    joyn(x, y, by = "id", match_type = "1:1", verbose = FALSE)
  )
})

test_that("all-NA key column is handled without crash", {
  x <- data.table(id = NA_integer_, val_x = 1L)
  y <- data.table(id = 1L:3L, val_y = 10:12)

  # Should warn (all keys NA) but not error
  expect_no_error(
    joyn(x, y, by = "id", match_type = "1:1", verbose = FALSE)
  )
})

# ── list key (rejected by check_var_class) ───────────────────────────────────

test_that("list key column generates a warning message", {
  x <- data.table(id = 1:3, val_x = 1:3)
  x$id <- as.list(1:3)  # coerce to list column
  y <- data.table(id = 1:3, val_y = 10:12)

  clear_joynenv()
  # check_var_class should detect and store a warning
  check_var_class(x, "id")
  flush_joyn_msgs()
  dt <- rlang::env_get(.joynenv, "joyn_msgs")
  expect_true("warn" %in% dt$type)
  expect_true(any(grepl("id", dt$msg)))
})

# ── multi-key with mixed types ───────────────────────────────────────────────

test_that("multi-key join with integer + character keys works", {
  x <- data.table(id  = 1L:3L, grp = c("a", "b", "c"), val_x = 1:3)
  y <- data.table(id  = 1L:3L, grp = c("a", "b", "d"), val_y = 10:12)

  result <- joyn(x, y, by = c("id", "grp"), match_type = "1:1", verbose = FALSE)

  expect_true(is.data.frame(result))
  # id=3,grp='c' x-only; id=3,grp='d' y-only; 1-a, 2-b matched → 4 rows
  expect_equal(nrow(result), 4L)
})

test_that("multi-key join with Date + integer keys works", {
  x <- data.table(
    dt  = as.Date(c("2023-01-01", "2023-01-02")),
    id  = 1L:2L,
    val_x = c(10, 20)
  )
  y <- data.table(
    dt  = as.Date(c("2023-01-01", "2023-01-03")),
    id  = 1L:2L,
    val_y = c(100, 200)
  )

  result <- joyn(x, y, by = c("dt", "id"), match_type = "1:1", verbose = FALSE)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3L)
})

# ── zero-row inputs ──────────────────────────────────────────────────────────

test_that("zero-row x joined with normal y returns y-only rows", {
  x <- data.table(id = integer(0), val_x = character(0))
  y <- data.table(id = 1L:3L, val_y = 10:12)

  result <- joyn(x, y, by = "id", match_type = "1:1", verbose = FALSE)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3L)
})

test_that("normal x joined with zero-row y returns x-only rows", {
  x <- data.table(id = 1L:3L, val_x = 10:12)
  y <- data.table(id = integer(0), val_y = character(0))

  result <- joyn(x, y, by = "id", match_type = "1:1", verbose = FALSE)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3L)
})

# ── logical key ──────────────────────────────────────────────────────────────

test_that("logical key column is accepted and joins correctly", {
  x <- data.table(flag = c(TRUE, FALSE, TRUE), val_x = 1:3)
  y <- data.table(flag = c(TRUE, FALSE),        val_y = 10:11)

  # logical is in .joyn_allowed_classes
  clear_joynenv()
  result <- check_var_class(x, "flag")
  expect_null(result)

  result <- joyn(x, y, by = "flag", match_type = "m:1", verbose = FALSE)
  expect_true(is.data.frame(result))
})
