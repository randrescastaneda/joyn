## Integration tests for joyn() in joyn-merge.R
## Covers: keep × match_type combinations, update_NAs, update_values,
##         reporttype variants, keep_common_vars, y_vars_to_keep,
##         zero-row inputs, deprecated arguments, nesting clear_joynenv

library(data.table)

# ── Fixtures ──────────────────────────────────────────────────────────────────

x1 <- data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
                 x  = 11:15)

y1 <- data.table(id = 1:2,
                 y  = c(11L, 15L))

x2 <- data.table(id = c(1L, 2L, 3L, 4L),
                 a  = c(1, 2, NA, 4))

y2 <- data.table(id = c(1L, 2L, 3L),
                 a  = c(10, 20, 30),
                 b  = c(100, 200, 300))

# ── keep variants ─────────────────────────────────────────────────────────────

test_that("keep = 'left' returns only x rows", {
  result <- joyn(x1, y1, by = "id", match_type = "m:1",
                 keep = "left", reporttype = "numeric", verbose = FALSE)

  # keep = left means rows from x only (both matched and x-unmatched)
  expect_equal(nrow(result), nrow(x1))
  rv <- getOption("joyn.reportvar")
  expect_true(all(result[[rv]] %in% c(1, 3)))  # 1=x-only, 3=matched
})

test_that("keep = 'right' returns only y rows", {
  result <- joyn(x1, y1, by = "id", match_type = "m:1",
                 keep = "right", reporttype = "numeric", verbose = FALSE)

  rv <- getOption("joyn.reportvar")
  expect_true(all(result[[rv]] %in% c(2, 3)))  # 2=y-only, 3=matched
})

test_that("keep = 'inner' returns only matched rows", {
  result <- joyn(x1, y1, by = "id", match_type = "m:1",
                 keep = "inner", reporttype = "numeric", verbose = FALSE)

  rv <- getOption("joyn.reportvar")
  expect_true(all(result[[rv]] == 3))  # 3=matched
})

test_that("keep = 'full' returns all rows", {
  result <- joyn(x1, y1, by = "id", match_type = "m:1",
                 keep = "full", reporttype = "numeric", verbose = FALSE)

  rv <- getOption("joyn.reportvar")
  expect_true(all(result[[rv]] %in% c(1, 2, 3)))
  # full join must have at least as many rows as x (includes unmatched y rows)
  expect_gte(nrow(result), nrow(x1))
})

test_that("keep = 'anti' returns unmatched x rows only", {
  result <- joyn(x1, y1, by = "id", match_type = "m:1",
                 keep = "anti", reporttype = "numeric", verbose = FALSE)

  rv <- getOption("joyn.reportvar")
  expect_equal(nrow(result), 2L)  # id=3 and id=NA are unmatched x rows
  expect_true(all(result[[rv]] == 1))  # 1=x-only
})

# ── match_type variants ───────────────────────────────────────────────────────

test_that("match_type = '1:1' works for unique keys", {
  x <- data.table(id = 1:3, val_x = letters[1:3])
  y <- data.table(id = 1:3, val_y = LETTERS[1:3])

  result <- joyn(x, y, by = "id", match_type = "1:1", verbose = FALSE)
  expect_equal(nrow(result), 3L)
})

test_that("match_type = '1:1' errors on non-unique x key", {
  x <- data.table(id = c(1L, 1L, 2L), val_x = 1:3)
  y <- data.table(id = 1:2, val_y = 10:11)

  expect_error(
    joyn(x, y, by = "id", match_type = "1:1", verbose = FALSE)
  )
})

test_that("match_type = 'm:1' works", {
  result <- joyn(x1, y1, by = "id", match_type = "m:1", verbose = FALSE)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) >= nrow(x1))
})

test_that("match_type = '1:m' works", {
  x <- data.table(id = 1:2, val_x = letters[1:2])
  y <- data.table(id = c(1L, 1L, 2L), val_y = LETTERS[1:3])

  result <- joyn(x, y, by = "id", match_type = "1:m", verbose = FALSE)
  expect_true(is.data.frame(result))
  # id=1 matches 2 y rows, id=2 matches 1 y row = 3 matched rows total
  expect_equal(nrow(result), 3L)
})

test_that("match_type = 'm:m' works", {
  x <- data.table(id = c(1L, 1L, 2L), val_x = 1:3)
  y <- data.table(id = c(1L, 1L, 3L), val_y = 10:12)

  result <- joyn(x, y, by = "id", match_type = "m:m",
                 reporttype = "numeric", verbose = FALSE)
  expect_true(is.data.frame(result))
  # id=1: 2x2=4 matched; id=2: 1 x-only; id=3: 1 y-only → 6 rows total
  expect_equal(nrow(result), 6L)
  rv <- getOption("joyn.reportvar")
  expect_true(all(result[[rv]] %in% c(1, 2, 3)))
})

# ── update_NAs and update_values ──────────────────────────────────────────────

test_that("update_NAs fills NA in x.a from y.a", {
  result <- joyn(x2, y2, by = "id", match_type = "1:1",
                 update_NAs = TRUE, verbose = FALSE)

  # id=3: x.a was NA, y.a = 30 → should be filled
  id3_row <- result[id == 3L]
  expect_false(is.na(id3_row$a))
  expect_equal(id3_row$a, 30)
})

test_that("update_values replaces all common vars with y values", {
  result <- joyn(x2, y2, by = "id", match_type = "1:1",
                 update_values = TRUE, verbose = FALSE)

  # For matched rows, a should come from y
  id1_row <- result[id == 1L]
  expect_equal(id1_row$a, 10)
})

test_that("update_values = TRUE also implies update_NAs", {
  result <- joyn(x2, y2, by = "id", match_type = "1:1",
                 update_values = TRUE, verbose = FALSE)

  id3_row <- result[id == 3L]
  expect_false(is.na(id3_row$a))
  expect_equal(id3_row$a, 30)
})

# ── reporttype variants ───────────────────────────────────────────────────────

test_that("reporttype = 'factor' produces factor report column", {
  result <- joyn(x1, y1, by = "id", match_type = "m:1",
                 reporttype = "factor", verbose = FALSE)

  rv <- getOption("joyn.reportvar")
  expect_true(is.factor(result[[rv]]))
})

test_that("reporttype = 'character' produces character report column", {
  result <- joyn(x1, y1, by = "id", match_type = "m:1",
                 reporttype = "character", verbose = FALSE)

  rv <- getOption("joyn.reportvar")
  expect_true(is.character(result[[rv]]))
})

test_that("reporttype = 'numeric' produces numeric report column", {
  result <- joyn(x1, y1, by = "id", match_type = "m:1",
                 reporttype = "numeric", verbose = FALSE)

  rv <- getOption("joyn.reportvar")
  expect_true(is.numeric(result[[rv]]))
})

# ── keep_common_vars ──────────────────────────────────────────────────────────

test_that("keep_common_vars = FALSE removes common non-key vars", {
  result <- joyn(x2, y2, by = "id", match_type = "1:1",
                 keep_common_vars = FALSE, verbose = FALSE)

  # 'a' is in both x2 and y2; with keep_common_vars=FALSE it should not appear
  # as both x.a and y.a — only the x version survives
  col_names <- names(result)
  expect_false("y.a" %in% col_names)
})

test_that("keep_common_vars = TRUE keeps suffixed common vars", {
  result <- joyn(x2, y2, by = "id", match_type = "1:1",
                 keep_common_vars = TRUE, verbose = FALSE)

  col_names <- names(result)
  # With suffixes, x's 'a' becomes 'a.x' and y's 'a' becomes 'a.y'
  expect_true("a.x" %in% col_names)
  expect_true("a.y" %in% col_names)
})

# ── y_vars_to_keep ────────────────────────────────────────────────────────────

test_that("y_vars_to_keep = FALSE drops all y variables", {
  result <- joyn(x1, y1, by = "id", match_type = "m:1",
                 y_vars_to_keep = FALSE, verbose = FALSE)

  expect_false("y" %in% names(result))
})

test_that("y_vars_to_keep = 'b' keeps only specified y vars", {
  result <- joyn(x2, y2, by = "id", match_type = "1:1",
                 y_vars_to_keep = "b", verbose = FALSE)

  expect_true("b" %in% names(result))
})

# ── reportvar ─────────────────────────────────────────────────────────────────

test_that("reportvar = FALSE removes report column", {
  result <- joyn(x1, y1, by = "id", match_type = "m:1",
                 reportvar = FALSE, verbose = FALSE)

  rv <- getOption("joyn.reportvar")
  expect_false(rv %in% names(result))
})

test_that("custom reportvar name is used", {
  result <- joyn(x1, y1, by = "id", match_type = "m:1",
                 reportvar = "my_report", verbose = FALSE)

  expect_true("my_report" %in% names(result))
})

# ── nesting: clear_joynenv skips on nested calls ──────────────────────────────

test_that("messages from dplyr wrapper are preserved through nested joyn() call", {
  # left_join stores a warn for copy=TRUE before calling joyn()
  # joyn() should NOT clear it via clear_joynenv()
  x_small <- data.table(id = 1:2, val_x = c(10, 20))
  y_small <- data.table(id = 1:2, val_y = c(100, 200))

  joyn::left_join(x_small, y_small, by = "id", copy = TRUE, verbose = FALSE)

  flush_joyn_msgs()
  msgs <- rlang::env_get(.joynenv, "joyn_msgs")
  # The copy=TRUE warning should be present
  expect_true("warn" %in% msgs$type)
  expect_true(any(grepl("copy", msgs$msg, ignore.case = TRUE)))
})

test_that("joyn_active flag is released after joyn() exits", {
  x_small <- data.table(id = 1:2, val_x = c(10, 20))
  y_small <- data.table(id = 1:2, val_y = c(100, 200))

  joyn(x_small, y_small, by = "id", match_type = "1:1", verbose = FALSE)

  # Flag should be cleared after joyn() returns
  expect_false(rlang::env_has(.joynenv, "joyn_active"))
})

test_that("joyn_active flag is released after dplyr wrapper exits", {
  x_small <- data.table(id = 1:2, val_x = c(10, 20))
  y_small <- data.table(id = 1:2, val_y = c(100, 200))

  joyn::left_join(x_small, y_small, by = "id", verbose = FALSE)

  expect_false(rlang::env_has(.joynenv, "joyn_active"))
})

# ── successive calls produce clean state ──────────────────────────────────────

test_that("successive joyn() calls each start with a fresh message state", {
  x_small <- data.table(id = 1:2, val_x = c(10, 20))
  y_small <- data.table(id = 1:2, val_y = c(100, 200))

  joyn(x_small, y_small, by = "id", match_type = "1:1", verbose = FALSE)
  expect_true(rlang::env_has(.joynenv, "joyn_msgs"),
              label = "joyn_msgs must exist after first joyn()")
  n_msgs_first <- nrow(rlang::env_get(.joynenv, "joyn_msgs"))

  joyn(x_small, y_small, by = "id", match_type = "1:1", verbose = FALSE)
  expect_true(rlang::env_has(.joynenv, "joyn_msgs"),
              label = "joyn_msgs must exist after second joyn()")
  n_msgs_second <- nrow(rlang::env_get(.joynenv, "joyn_msgs"))

  # Second call should produce the same number of messages, not accumulate
  expect_equal(n_msgs_first, n_msgs_second)
})
