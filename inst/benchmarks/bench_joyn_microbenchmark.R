# joyn checkpointed microbenchmark runner
#
# Run from a shell, not from the interactive R console:
#   Rscript inst/benchmarks/bench_joyn_microbenchmark.R
#
# Useful environment variables:
#   JOYN_BENCH_SMOKE=true
#   JOYN_BENCH_SIZES=10000,100000,500000,1000000
#   JOYN_BENCH_REPS_SMALL=20
#   JOYN_BENCH_REPS_MEDIUM=10
#   JOYN_BENCH_REPS_LARGE=3
#   JOYN_BENCH_REPS_XL=1
#   JOYN_BENCH_OUTDIR=inst/benchmarks/results

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

  sizes <- suppressWarnings(as.numeric(strsplit(value, ",", fixed = TRUE)[[1]]))
  if (any(is.na(sizes)) || any(sizes < 1)) {
    stop("JOYN_BENCH_SIZES must be comma-separated positive row counts.", call. = FALSE)
  }
  as.integer(sizes)
}

format_size <- function(n) {
  formatC(n, format = "d", big.mark = ",")
}

make_data <- function(n, seed = 42L) {
  set.seed(seed)
  list(
    x = data.table::data.table(
      id = sample.int(n, n, replace = FALSE),
      val = rnorm(n)
    ),
    y = data.table::data.table(
      id = sample.int(n, n, replace = FALSE),
      grp = sample(letters[1:5], n, replace = TRUE)
    ),
    by = "id"
  )
}

reps_for_size <- function(n) {
  if (n <= 1e4) return(parse_int("JOYN_BENCH_REPS_SMALL", 20L))
  if (n <= 1e5) return(parse_int("JOYN_BENCH_REPS_MEDIUM", 10L))
  if (n <= 5e5) return(parse_int("JOYN_BENCH_REPS_LARGE", 3L))
  parse_int("JOYN_BENCH_REPS_XL", 1L)
}

save_checkpoint <- function(checkpoint, outdir, benchmark_name, n) {
  path <- file.path(
    outdir,
    sprintf(
      "%s-n%s-%s.rds",
      benchmark_name,
      formatC(n, format = "d", big.mark = ""),
      format(Sys.time(), "%Y%m%d-%H%M%S")
    )
  )
  saveRDS(checkpoint, path)
  log_line("saved: ", path)
  invisible(path)
}

run_one <- function(name, expression, n, times, outdir) {
  log_line("start: ", name, " | n=", format_size(n), " | times=", times)
  gc(verbose = FALSE)

  started_at <- Sys.time()
  result <- microbenchmark::microbenchmark(
    eval(expression),
    times = times,
    unit = "ms",
    control = list(warmup = 1L)
  )

  checkpoint <- list(
    benchmark = name,
    n = n,
    times = times,
    unit = "ms",
    started_at = started_at,
    completed_at = Sys.time(),
    result = result,
    summary = summary(result),
    session_info = utils::sessionInfo()
  )

  save_checkpoint(checkpoint, outdir, name, n)
  log_line("done: ", name)
  checkpoint
}

smoke <- parse_bool("JOYN_BENCH_SMOKE", FALSE)
outdir <- Sys.getenv("JOYN_BENCH_OUTDIR", unset = "inst/benchmarks/results")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

sizes <- if (smoke) 1000L else parse_sizes()

log_line("loading joyn via devtools::load_all()")
devtools::load_all(".", quiet = TRUE)
log_line("output directory: ", outdir)
log_line("sizes: ", paste(format_size(sizes), collapse = ", "))
log_line("smoke mode: ", smoke)

all_summaries <- list()

for (n in sizes) {
  data <- make_data(n)
  times <- if (smoke) 1L else reps_for_size(n)

  benchmarks <- list(
    joyn_1to1 = quote(joyn(data$x, data$y, by = data$by, match_type = "1:1", verbose = FALSE)),
    left_join = quote(left_join(data$x, data$y, by = data$by, verbose = FALSE)),
    right_join = quote(right_join(data$x, data$y, by = data$by, verbose = FALSE)),
    full_join = quote(full_join(data$x, data$y, by = data$by, verbose = FALSE)),
    inner_join = quote(inner_join(data$x, data$y, by = data$by, verbose = FALSE)),
    anti_join = quote(anti_join(data$x, data$y, by = data$by, verbose = FALSE))
  )

  for (benchmark_name in names(benchmarks)) {
    checkpoint <- run_one(benchmark_name, benchmarks[[benchmark_name]], n, times, outdir)
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
