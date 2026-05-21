#!/usr/bin/env Rscript
# joyn checkpointed microbenchmark runner
#
# Must be run from the package root directory (the folder containing DESCRIPTION).
#
# Run from a shell, not from the interactive R console:
#   Rscript inst/benchmarks/bench_joyn_microbenchmark.R
#
# Useful environment variables (with defaults):
#   JOYN_BENCH_SMOKE=true           # default: false
#   JOYN_BENCH_SIZES=10000,...      # default: 10000,100000,500000,1000000
#   JOYN_BENCH_REPS_SMALL=20        # default: 20
#   JOYN_BENCH_REPS_MEDIUM=10       # default: 10
#   JOYN_BENCH_REPS_LARGE=3         # default: 3
#   JOYN_BENCH_REPS_XL=1            # default: 1
#   JOYN_BENCH_OUTDIR=...           # default: inst/benchmarks/results

if (!file.exists("DESCRIPTION")) {
  stop(
    "Must be run from the package root (directory containing DESCRIPTION).",
    call. = FALSE
  )
}

required_packages <- c("data.table", "devtools", "microbenchmark")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

log_line <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", ..., "\n", sep = "")
  flush.console()
}

parse_bool <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = if (default) "true" else "false")
  tolower(value) %in% c("1", "true", "yes", "y")
}

parse_int <- function(name, default) {
  value <- Sys.getenv(name, unset = as.character(default))
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed) || parsed < 1L) {
    stop("Environment variable ", name, " must be a positive integer.", call. = FALSE)
  }
  parsed
}

parse_sizes <- function() {
  value <- Sys.getenv("JOYN_BENCH_SIZES", unset = "")
  if (!nzchar(value)) {
    return(as.integer(c(1e4, 1e5, 5e5, 1e6)))
  }

  sizes_raw <- strsplit(value, ",", fixed = TRUE)[[1]]
  sizes     <- suppressWarnings(as.numeric(sizes_raw))

  if (any(is.na(sizes)) || any(sizes < 1)) {
    stop("JOYN_BENCH_SIZES must be comma-separated positive row counts.", call. = FALSE)
  }
  if (any(sizes != trunc(sizes))) {
    stop(
      "JOYN_BENCH_SIZES values must be whole numbers, got: ",
      paste(sizes[sizes != trunc(sizes)], collapse = ", "),
      call. = FALSE
    )
  }
  if (any(sizes > .Machine$integer.max)) {
    stop(
      "JOYN_BENCH_SIZES values exceed maximum integer (", .Machine$integer.max, ").",
      call. = FALSE
    )
  }
  as.integer(sizes)
}

format_size <- function(n) {
  formatC(n, format = "d", big.mark = ",")
}

make_data <- function(n, seed = 42L) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1L) {
    stop("`n` must be a single positive integer.", call. = FALSE)
  }
  n <- as.integer(n)
  set.seed(seed)
  # Both tables get unique integer `id` keys — exercises the 1:1 join path.
  # x$id and y$id are different permutations of 1:n because the two
  # sample.int() calls consume successive RNG states from the same seed.
  list(
    x = data.table::data.table(
      id  = sample.int(n),
      val = rnorm(n)
    ),
    y = data.table::data.table(
      id  = sample.int(n),
      grp = sample(letters[1:5], n, replace = TRUE)
    ),
    by = "id"
  )
}

reps_for_size <- function(n) {
  if (n <= 1e4) return(parse_int("JOYN_BENCH_REPS_SMALL",  20L))
  if (n <= 1e5) return(parse_int("JOYN_BENCH_REPS_MEDIUM", 10L))
  if (n <= 5e5) return(parse_int("JOYN_BENCH_REPS_LARGE",   3L))
  parse_int("JOYN_BENCH_REPS_XL", 1L)
}

save_checkpoint <- function(checkpoint, outdir, benchmark_name, n) {
  path <- file.path(
    outdir,
    sprintf(
      "%s-n%s-%s.rds",
      benchmark_name,
      formatC(n, format = "d", big.mark = ""),
      format(checkpoint$started_at, "%Y%m%d-%H%M%S")
    )
  )
  saveRDS(checkpoint, path)
  log_line("saved: ", path)
  invisible(path)
}

# Runs a single microbenchmark, saves a .rds checkpoint, and returns the
# checkpoint list. `expression` must be a quoted call. Evaluates in `envir`.
run_one <- function(name, expression, n, times, outdir, session_info,
                    envir = parent.frame()) {
  log_line("start: ", name, " | n=", format_size(n), " | times=", times)

  started_at <- Sys.time()
  result <- microbenchmark::microbenchmark(
    eval(expression, envir = envir),
    times   = times,
    unit    = "ms",
    control = list(warmup = 1L)
  )

  checkpoint <- list(
    benchmark    = name,
    n            = n,
    times        = times,
    unit         = "ms",
    started_at   = started_at,
    completed_at = Sys.time(),
    result       = result,
    summary      = summary(result),
    session_info = session_info
  )

  save_checkpoint(checkpoint, outdir, name, n)
  log_line("done:  ", name)
  checkpoint
}

# ---------------------------------------------------------------------------
smoke  <- parse_bool("JOYN_BENCH_SMOKE", FALSE)
outdir <- Sys.getenv("JOYN_BENCH_OUTDIR", unset = "inst/benchmarks/results")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

sizes <- if (smoke) 1000L else parse_sizes()

log_line("loading joyn via devtools::load_all()")
devtools::load_all(".", quiet = TRUE)
log_line("resolved outdir: ", normalizePath(outdir, mustWork = FALSE))
log_line("sizes: ",           paste(format_size(sizes), collapse = ", "))
log_line("smoke mode: ",      smoke)

# Capture once — identical for all benchmarks in this run.
session <- utils::sessionInfo()

# These symbols resolve to joyn's own exports, loaded above via load_all().
# Do not attach dplyr or any other package that exports identically-named functions.
benchmarks <- list(
  joyn_1to1  = quote(joyn(data$x, data$y, by = data$by, match_type = "1:1", verbose = FALSE)),
  left_join  = quote(left_join(data$x,  data$y, by = data$by, verbose = FALSE)),
  right_join = quote(right_join(data$x, data$y, by = data$by, verbose = FALSE)),
  full_join  = quote(full_join(data$x,  data$y, by = data$by, verbose = FALSE)),
  inner_join = quote(inner_join(data$x, data$y, by = data$by, verbose = FALSE)),
  anti_join  = quote(anti_join(data$x,  data$y, by = data$by, verbose = FALSE))
)

# Pre-allocate result collector.
all_keys      <- as.vector(outer(names(benchmarks), sizes, paste, sep = "_"))
all_summaries <- vector("list", length(all_keys))
names(all_summaries) <- all_keys

for (n in sizes) {
  data  <- make_data(n)
  times <- if (smoke) 1L else reps_for_size(n)

  for (benchmark_name in names(benchmarks)) {
    checkpoint <- run_one(
      name         = benchmark_name,
      expression   = benchmarks[[benchmark_name]],
      n            = n,
      times        = times,
      outdir       = outdir,
      session_info = session,
      envir        = environment()
    )
    all_summaries[[paste(benchmark_name, n, sep = "_")]] <- checkpoint$summary
  }

  rm(data)
  gc(verbose = FALSE)
}

summary_path <- file.path(
  outdir,
  sprintf("summary-%s.rds", format(Sys.time(), "%Y%m%d-%H%M%S"))
)
saveRDS(all_summaries, summary_path)
log_line("saved summary: ", summary_path)
log_line("benchmark run complete")

invisible(all_summaries)
