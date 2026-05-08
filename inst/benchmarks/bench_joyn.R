# joyn Benchmark Suite
#
# Purpose: Measure performance of joyn functions using microbenchmark.
#          Covers small (1K–100K), large (500K), and very large (1M) sizes.
#
# Location: inst/benchmarks/ — NOT in tests/ (microbenchmark is in Suggests).
#
# Usage:
#   devtools::load_all(); source("inst/benchmarks/bench_joyn.R")
#   # or: Rscript inst/benchmarks/bench_joyn.R
#
# ===========================================================================
# BASELINE TIMINGS (recorded 2026-05-08, before optimization)
# Platform: macOS, R 4.6.0, collapse 2.1.6, data.table 1.18.4
#
# store_msg() x50:          23.6ms
# check_var_class() x10:    175µs (int), 153µs (chr)
# joyn_workhorse() 10K:     4.57ms
# joyn() 1K  (1:1):         24.6ms
# joyn() 10K (1:1):         18.0ms
# joyn() 100K (1:1):        53.7ms
# left_join()  10K:         22.7ms
# right_join() 10K:         22.5ms
# full_join()  10K:         22.3ms
# inner_join() 10K:         23.6ms
# anti_join()  10K:         17.1ms
#
# POST-OPTIMIZATION TIMINGS (recorded 2026-05-08, after Phase 1-4 work)
#
# store_msg() x50:          21.2ms  (-11%)
# check_var_class() x10:    171µs (int), 141µs (chr)
# joyn_workhorse() 10K:     3.37ms  (-26%)
# joyn() 1K  (1:1):         24.5ms
# joyn() 10K (1:1):         17.4ms  (-3%)
# joyn() 100K (1:1):        41.3ms  (-23%)
# left_join()  10K:         20.0ms  (-12%)
# right_join() 10K:         22.7ms
# full_join()  10K:         20.6ms  (-8%)
# inner_join() 10K:         28.9ms
# anti_join()  10K:         17.9ms
# ===========================================================================

# -- Dependencies ------------------------------------------------------------
if (!requireNamespace("microbenchmark", quietly = TRUE)) {
  stop(
    "Package 'microbenchmark' is required. Install with:\n",
    "  install.packages('microbenchmark')",
    call. = FALSE
  )
}
library(microbenchmark)
library(data.table)

if (!requireNamespace("joyn", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Install 'devtools' to load joyn from source.", call. = FALSE)
  }
  message("Loading joyn from source via devtools::load_all()...")
  devtools::load_all(".")
} else {
  library(joyn)
}

# -- Helpers -----------------------------------------------------------------

make_data <- function(n, n_keys = 1, seed = 42) {
  set.seed(seed)
  if (n_keys == 1) {
    x <- data.table(id  = sample(seq_len(n), n, replace = FALSE),
                    val = rnorm(n))
    y <- data.table(id  = sample(seq_len(n), n, replace = FALSE),
                    grp = sample(letters[1:5], n, replace = TRUE))
    list(x = x, y = y, by1 = "id")
  } else {
    x <- data.table(id1 = sample(seq_len(ceiling(n / 10)), n, replace = TRUE),
                    id2 = sample(seq_len(10), n, replace = TRUE),
                    id3 = sample(c("A","B","C"), n, replace = TRUE),
                    val = rnorm(n))
    y <- data.table(id1 = sample(seq_len(ceiling(n / 10)), n, replace = TRUE),
                    id2 = sample(seq_len(10), n, replace = TRUE),
                    id3 = sample(c("A","B","C"), n, replace = TRUE),
                    grp = sample(letters[1:5], n, replace = TRUE))
    list(x = x, y = y, by1 = c("id1","id2","id3"))
  }
}

print_mb <- function(label, mb, unit = "ms") {
  s  <- summary(mb)
  cat(sprintf("  %-50s  median: %7.2f %s  (min: %.2f, max: %.2f, neval: %d)\n",
              label,
              s$median, unit,
              s$min, s$max, s$neval))
}

sep <- function(title) {
  cat("\n", strrep("=", 70), "\n", title, "\n", strrep("=", 70), "\n\n", sep = "")
}

# -- Configuration -----------------------------------------------------------
NEVAL_INTERNAL <- 50   # fast internals — many reps
NEVAL_SMALL    <- 30   # 1K–10K joins
NEVAL_MEDIUM   <- 20   # 100K joins
NEVAL_LARGE    <- 10   # 500K joins
NEVAL_XL       <-  5   # 1M joins  (takes longest — be patient)

# ---------------------------------------------------------------------------
sep("SECTION 1: INTERNAL FUNCTIONS")
# ---------------------------------------------------------------------------

cat("1a. store_msg() — 50 sequential calls:\n")
mb_store <- microbenchmark(
  store_msg_x50 = {
    joyn:::clear_joynenv()
    for (i in seq_len(50))
      joyn:::store_joyn_msg(info = paste("benchmark message number", i))
  },
  times = NEVAL_INTERNAL, unit = "ms"
)
print_mb("store_msg x50", mb_store)

cat("\n1b. check_var_class() — 10 calls, 1 key variable:\n")
dt_int <- data.table(id = 1:1000L,        val = rnorm(1000))
dt_chr <- data.table(id = as.character(1:1000), val = rnorm(1000))

mb_cvc_int <- microbenchmark(
  check_var_class_int = { for (i in seq_len(10)) joyn:::check_var_class(dt_int, "id") },
  times = NEVAL_INTERNAL, unit = "us"
)
mb_cvc_chr <- microbenchmark(
  check_var_class_chr = { for (i in seq_len(10)) joyn:::check_var_class(dt_chr, "id") },
  times = NEVAL_INTERNAL, unit = "us"
)
print_mb("check_var_class x10 (integer)",   mb_cvc_int, "µs")
print_mb("check_var_class x10 (character)", mb_cvc_chr, "µs")

cat("\n1c. joyn_workhorse() — 10K rows, 1 key:\n")
d10k <- make_data(1e4)
mb_wh <- microbenchmark(
  joyn_workhorse_10k = joyn:::joyn_workhorse(d10k$x, d10k$y, by = d10k$by1),
  times = NEVAL_SMALL, unit = "ms"
)
print_mb("joyn_workhorse 10K (1 key)", mb_wh)

# ---------------------------------------------------------------------------
sep("SECTION 2: joyn() END-TO-END — BY SIZE")
# ---------------------------------------------------------------------------

size_cfg <- list(
  list(n = 1e3,  neval = NEVAL_SMALL,  unit = "ms"),
  list(n = 1e4,  neval = NEVAL_SMALL,  unit = "ms"),
  list(n = 1e5,  neval = NEVAL_MEDIUM, unit = "ms"),
  list(n = 5e5,  neval = NEVAL_LARGE,  unit = "ms"),
  list(n = 1e6,  neval = NEVAL_XL,     unit = "ms")
)

for (cfg in size_cfg) {
  n     <- cfg$n
  neval <- cfg$neval
  unit  <- cfg$unit
  lbl   <- formatC(n, format = "d", big.mark = ",")
  d     <- make_data(n)

  cat(sprintf("--- n = %s rows (neval = %d) ---\n", lbl, neval))

  mb_11 <- microbenchmark(
    joyn_1to1 = joyn(d$x, d$y, by = d$by1, match_type = "1:1", verbose = FALSE),
    times = neval, unit = unit
  )
  mb_mm <- microbenchmark(
    joyn_mtom = joyn(d$x, d$y, by = d$by1, match_type = "m:m", verbose = FALSE),
    times = neval, unit = unit
  )

  print_mb(sprintf("joyn() 1:1 (%s, 1 key)", lbl), mb_11, unit)
  print_mb(sprintf("joyn() m:m (%s, 1 key)", lbl), mb_mm, unit)
  cat("\n")
}

# 3-key join at 10K and 500K
for (n in c(1e4, 5e5)) {
  neval <- if (n == 1e4) NEVAL_SMALL else NEVAL_LARGE
  d3    <- make_data(n, n_keys = 3)
  lbl   <- formatC(n, format = "d", big.mark = ",")
  cat(sprintf("--- 3-key m:m, n = %s (neval = %d) ---\n", lbl, neval))
  mb_3k <- microbenchmark(
    joyn_3key = joyn(d3$x, d3$y, by = d3$by1, match_type = "m:m", verbose = FALSE),
    times = neval, unit = "ms"
  )
  print_mb(sprintf("joyn() m:m (%s, 3 keys)", lbl), mb_3k)
  cat("\n")
}

# ---------------------------------------------------------------------------
sep("SECTION 3: DPLYR WRAPPERS — BY SIZE")
# ---------------------------------------------------------------------------

wrapper_cfg <- list(
  list(n = 1e4, neval = NEVAL_SMALL,  label = "10K"),
  list(n = 5e5, neval = NEVAL_LARGE,  label = "500K"),
  list(n = 1e6, neval = NEVAL_XL,     label = "1M")
)

for (cfg in wrapper_cfg) {
  n     <- cfg$n
  neval <- cfg$neval
  lbl   <- cfg$label
  d     <- make_data(n)

  cat(sprintf("--- dplyr wrappers, n = %s (neval = %d) ---\n", lbl, neval))

  wrappers <- list(
    list(fn = "left_join",  call = quote(left_join(d$x,  d$y, by = d$by1, verbose = FALSE))),
    list(fn = "right_join", call = quote(right_join(d$x, d$y, by = d$by1, verbose = FALSE))),
    list(fn = "full_join",  call = quote(full_join(d$x,  d$y, by = d$by1, verbose = FALSE))),
    list(fn = "inner_join", call = quote(inner_join(d$x, d$y, by = d$by1, verbose = FALSE))),
    list(fn = "anti_join",  call = quote(anti_join(d$x,  d$y, by = d$by1, verbose = FALSE)))
  )

  for (w in wrappers) {
    mb <- microbenchmark(expr = eval(w$call), times = neval, unit = "ms")
    print_mb(sprintf("%s %s", w$fn, lbl), mb)
  }
  cat("\n")
}

# ---------------------------------------------------------------------------
sep("BENCHMARK COMPLETE")
# ---------------------------------------------------------------------------
cat("Re-run after future optimizations and update the BASELINE comment block.\n\n")
