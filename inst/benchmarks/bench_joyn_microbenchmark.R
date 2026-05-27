#!/usr/bin/env Rscript
# joyn structured microbenchmark runner
#
# Must be run from the package root directory (the folder containing DESCRIPTION).
#
# Run from a shell, not from the interactive R console:
#   Rscript inst/benchmarks/bench_joyn_microbenchmark.R \
#     > inst/benchmarks/bench_joyn_microbenchmark.log 2>&1
#
# Useful environment variables (with defaults):
#   JOYN_BENCH_SMOKE=true           # default: false
#   JOYN_BENCH_SIZES=10000,...      # default: 10000,100000,500000,1000000
#   JOYN_BENCH_REPS_SMALL=30        # default: 30
#   JOYN_BENCH_REPS_MEDIUM=20       # default: 20
#   JOYN_BENCH_REPS_LARGE=10        # default: 10
#   JOYN_BENCH_REPS_XL=5            # default: 5
#   JOYN_BENCH_OVERLAP=0.8          # default: 0.8
#   JOYN_BENCH_OUTDIR=...           # default: inst/benchmarks/results

if (!file.exists("DESCRIPTION")) {
  stop(
    "Must be run from the package root (directory containing DESCRIPTION).",
    call. = FALSE
  )
}

required_packages <- c("collapse", "data.table", "devtools", "dplyr", "microbenchmark")
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

parse_double <- function(name, default) {
  value <- Sys.getenv(name, unset = as.character(default))
  parsed <- suppressWarnings(as.numeric(value))
  if (is.na(parsed)) {
    stop("Environment variable ", name, " must be numeric.", call. = FALSE)
  }
  parsed
}

parse_sizes <- function() {
  value <- Sys.getenv("JOYN_BENCH_SIZES", unset = "")
  if (!nzchar(value)) {
    return(as.integer(c(1e4, 1e5, 5e5, 1e6)))
  }

  sizes_raw <- strsplit(value, ",", fixed = TRUE)[[1]]
  sizes <- suppressWarnings(as.numeric(trimws(sizes_raw)))

  if (any(is.na(sizes)) || any(sizes < 1)) {
    stop("JOYN_BENCH_SIZES must be comma-separated positive row counts.", call. = FALSE)
  }
  if (any(sizes != trunc(sizes))) {
    stop("JOYN_BENCH_SIZES values must be whole numbers.", call. = FALSE)
  }
  if (any(sizes > .Machine$integer.max)) {
    stop(
      "JOYN_BENCH_SIZES values exceed maximum integer (", .Machine$integer.max, ").",
      call. = FALSE
    )
  }

  as.integer(unique(sizes))
}

format_size <- function(n) {
  formatC(as.integer(n), format = "d", big.mark = ",")
}

format_join_label <- function(join_type) {
  paste0(join_type, " join")
}

parse_overlap <- function() {
  overlap <- parse_double("JOYN_BENCH_OVERLAP", 0.8)
  if (overlap <= 0 || overlap >= 1) {
    stop("JOYN_BENCH_OVERLAP must be strictly between 0 and 1.", call. = FALSE)
  }
  overlap
}

make_data <- function(n, overlap = 0.8, seed = 42L) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 2L) {
    stop("`n` must be a single integer >= 2.", call. = FALSE)
  }

  n <- as.integer(n)
  shared_n <- as.integer(floor(n * overlap))
  if (shared_n < 1L || shared_n >= n) {
    stop("Overlap yields invalid shared key count for n = ", n, ".", call. = FALSE)
  }

  set.seed(seed)

  shared_ids <- seq_len(shared_n)
  x_only_ids <- seq.int(shared_n + 1L, n)
  y_only_ids <- seq.int(n + 1L, n + (n - shared_n))

  x_ids <- sample(c(shared_ids, x_only_ids), size = n, replace = FALSE)
  y_ids <- sample(c(shared_ids, y_only_ids), size = n, replace = FALSE)

  x_dt <- data.table::data.table(
    id = x_ids,
    val = rnorm(n)
  )
  y_dt <- data.table::data.table(
    id = y_ids,
    grp = sample(letters[1:5], n, replace = TRUE)
  )

  list(
    x_dt = x_dt,
    y_dt = y_dt,
    x_df = as.data.frame(x_dt),
    y_df = as.data.frame(y_dt),
    by = "id",
    shared_n = shared_n,
    overlap = shared_n / n
  )
}

reps_for_size <- function(n) {
  if (n <= 1e4) return(parse_int("JOYN_BENCH_REPS_SMALL", 30L))
  if (n <= 1e5) return(parse_int("JOYN_BENCH_REPS_MEDIUM", 20L))
  if (n <= 5e5) return(parse_int("JOYN_BENCH_REPS_LARGE", 10L))
  parse_int("JOYN_BENCH_REPS_XL", 5L)
}

join_grid <- function() {
  data.frame(
    join_type = rep(c("left", "right", "full", "inner", "anti"), each = 5L),
    engine = rep(c("collapse", "dplyr", "joyn", "data.table", "base"), times = 5L),
    stringsAsFactors = FALSE
  )
}

tool_label <- function(engine, join_type) {
  switch(
    engine,
    collapse = "collapse::join",
    dplyr = paste0("dplyr::", join_type, "_join"),
    joyn = paste0("joyn::", join_type, "_join"),
    `data.table` = if (join_type == "anti") "data.table anti join" else "data.table::merge",
    base = if (join_type == "anti") "base anti join" else "base::merge",
    stop("Unsupported engine: ", engine, call. = FALSE)
  )
}

build_expression <- function(join_type, engine) {
  switch(
    engine,
    collapse = switch(
      join_type,
      left = quote(collapse::join(data$x_df, data$y_df, on = data$by, how = "left", verbose = FALSE)),
      right = quote(collapse::join(data$x_df, data$y_df, on = data$by, how = "right", verbose = FALSE)),
      full = quote(collapse::join(data$x_df, data$y_df, on = data$by, how = "full", verbose = FALSE)),
      inner = quote(collapse::join(data$x_df, data$y_df, on = data$by, how = "inner", verbose = FALSE)),
      anti = quote(collapse::join(data$x_df, data$y_df, on = data$by, how = "anti", verbose = FALSE)),
      stop("Unsupported join type: ", join_type, call. = FALSE)
    ),
    dplyr = switch(
      join_type,
      left = quote(dplyr::left_join(data$x_df, data$y_df, by = data$by)),
      right = quote(dplyr::right_join(data$x_df, data$y_df, by = data$by)),
      full = quote(dplyr::full_join(data$x_df, data$y_df, by = data$by)),
      inner = quote(dplyr::inner_join(data$x_df, data$y_df, by = data$by)),
      anti = quote(dplyr::anti_join(data$x_df, data$y_df, by = data$by)),
      stop("Unsupported join type: ", join_type, call. = FALSE)
    ),
    joyn = switch(
      join_type,
      left = quote(left_join(data$x_dt, data$y_dt, by = data$by, verbose = FALSE)),
      right = quote(right_join(data$x_dt, data$y_dt, by = data$by, verbose = FALSE)),
      full = quote(full_join(data$x_dt, data$y_dt, by = data$by, verbose = FALSE)),
      inner = quote(inner_join(data$x_dt, data$y_dt, by = data$by, verbose = FALSE)),
      anti = quote(anti_join(data$x_dt, data$y_dt, by = data$by, verbose = FALSE)),
      stop("Unsupported join type: ", join_type, call. = FALSE)
    ),
    `data.table` = switch(
      join_type,
      left = quote(base::merge(data$x_dt, data$y_dt, by = data$by, all.x = TRUE, sort = FALSE)),
      right = quote(base::merge(data$x_dt, data$y_dt, by = data$by, all.y = TRUE, sort = FALSE)),
      full = quote(base::merge(data$x_dt, data$y_dt, by = data$by, all = TRUE, sort = FALSE)),
      inner = quote(base::merge(data$x_dt, data$y_dt, by = data$by, sort = FALSE)),
      anti = quote(data$x_dt[!data$y_dt, on = data$by]),
      stop("Unsupported join type: ", join_type, call. = FALSE)
    ),
    base = switch(
      join_type,
      left = quote(base::merge(data$x_df, data$y_df, by = data$by, all.x = TRUE, sort = FALSE)),
      right = quote(base::merge(data$x_df, data$y_df, by = data$by, all.y = TRUE, sort = FALSE)),
      full = quote(base::merge(data$x_df, data$y_df, by = data$by, all = TRUE, sort = FALSE)),
      inner = quote(base::merge(data$x_df, data$y_df, by = data$by, sort = FALSE)),
      anti = quote(data$x_df[is.na(match(data$x_df[[data$by]], data$y_df[[data$by]])), , drop = FALSE]),
      stop("Unsupported join type: ", join_type, call. = FALSE)
    ),
    stop("Unsupported engine: ", engine, call. = FALSE)
  )
}

summarise_result <- function(result, join_type, engine, n, times, started_at, completed_at) {
  stats <- as.data.frame(summary(result), stringsAsFactors = FALSE)
  data.frame(
    join_type = join_type,
    join_label = format_join_label(join_type),
    engine = engine,
    tool = tool_label(engine, join_type),
    n = as.integer(n),
    times = as.integer(times),
    unit = "ms",
    min = stats$min,
    lq = stats$lq,
    mean = stats$mean,
    median = stats$median,
    uq = stats$uq,
    max = stats$max,
    started_at = as.character(started_at),
    completed_at = as.character(completed_at),
    stringsAsFactors = FALSE
  )
}

save_checkpoint <- function(checkpoint, outdir, run_id) {
  checkpoint_dir <- file.path(outdir, "checkpoints")
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

  path <- file.path(
    checkpoint_dir,
    paste0(
      checkpoint$join_type, "-",
      gsub("[^A-Za-z0-9]+", "-", checkpoint$engine),
      "-n", checkpoint$n,
      "-", run_id,
      ".rds"
    )
  )

  saveRDS(checkpoint, path)
  log_line("saved checkpoint: ", path)
  invisible(path)
}

run_one <- function(join_type, engine, data, n, times, outdir, run_id, envir = parent.frame()) {
  log_line(
    "start: ", format_join_label(join_type),
    " | engine=", engine,
    " | n=", format_size(n),
    " | times=", times
  )

  started_at <- Sys.time()
  expression <- build_expression(join_type, engine)
  result <- microbenchmark::microbenchmark(
    eval(expression, envir = envir),
    times = times,
    unit = "ms",
    control = list(warmup = 1L)
  )
  completed_at <- Sys.time()

  summary_row <- summarise_result(
    result = result,
    join_type = join_type,
    engine = engine,
    n = n,
    times = times,
    started_at = started_at,
    completed_at = completed_at
  )

  checkpoint <- list(
    join_type = join_type,
    engine = engine,
    tool = tool_label(engine, join_type),
    n = as.integer(n),
    times = as.integer(times),
    unit = "ms",
    started_at = started_at,
    completed_at = completed_at,
    result = result,
    summary = summary_row
  )

  save_checkpoint(checkpoint, outdir, run_id)
  log_line(
    "done:  ", format_join_label(join_type),
    " | engine=", engine,
    " | median=", sprintf("%.2f", summary_row$median), " ms"
  )

  checkpoint
}

write_artifacts <- function(results, metadata, outdir, run_id) {
  artifact <- list(
    metadata = metadata,
    results = results
  )

  latest_rds <- file.path(outdir, "benchmark-results-latest.rds")
  latest_csv <- file.path(outdir, "benchmark-results-latest.csv")
  snapshot_rds <- file.path(outdir, paste0("benchmark-results-", run_id, ".rds"))
  snapshot_csv <- file.path(outdir, paste0("benchmark-results-", run_id, ".csv"))

  saveRDS(artifact, latest_rds)
  utils::write.csv(results, latest_csv, row.names = FALSE)
  saveRDS(artifact, snapshot_rds)
  utils::write.csv(results, snapshot_csv, row.names = FALSE)

  log_line("saved artifact: ", latest_rds)
  log_line("saved artifact: ", latest_csv)
  log_line("saved snapshot: ", snapshot_rds)
  log_line("saved snapshot: ", snapshot_csv)

  invisible(artifact)
}

# ---------------------------------------------------------------------------
smoke <- parse_bool("JOYN_BENCH_SMOKE", FALSE)
outdir <- Sys.getenv("JOYN_BENCH_OUTDIR", unset = "inst/benchmarks/results")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

sizes <- if (smoke) 1000L else parse_sizes()
overlap <- parse_overlap()
run_id <- format(Sys.time(), "%Y%m%d-%H%M%S")

log_line("loading joyn via devtools::load_all()")
devtools::load_all(".", quiet = TRUE)
log_line("resolved outdir: ", normalizePath(outdir, mustWork = FALSE))
log_line("sizes: ", paste(format_size(sizes), collapse = ", "))
log_line("overlap: ", sprintf("%.2f", overlap))
log_line("smoke mode: ", smoke)

session <- utils::sessionInfo()
package_versions <- vapply(
  c("joyn", "collapse", "data.table", "dplyr", "microbenchmark"),
  function(pkg) as.character(utils::packageVersion(pkg)),
  FUN.VALUE = character(1)
)

specs <- join_grid()
results <- vector("list", length = nrow(specs) * length(sizes))
result_index <- 1L

for (n in sizes) {
  data <- make_data(n = n, overlap = overlap)
  times <- if (smoke) 1L else reps_for_size(n)

  log_line(
    "dataset ready: n=", format_size(n),
    " | shared keys=", format_size(data$shared_n),
    " | overlap=", sprintf("%.2f", data$overlap)
  )

  for (row_id in seq_len(nrow(specs))) {
    checkpoint <- run_one(
      join_type = specs$join_type[[row_id]],
      engine = specs$engine[[row_id]],
      data = data,
      n = n,
      times = times,
      outdir = outdir,
      run_id = run_id,
      envir = environment()
    )

    results[[result_index]] <- checkpoint$summary
    result_index <- result_index + 1L
  }

  rm(data)
  gc(verbose = FALSE)
}

results_df <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
results_df <- as.data.frame(results_df)
results_df <- results_df[order(results_df$join_type, results_df$engine, results_df$n), , drop = FALSE]
row.names(results_df) <- NULL

metadata <- list(
  generated_at = as.character(Sys.time()),
  run_id = run_id,
  smoke = smoke,
  sizes = as.integer(sizes),
  overlap = overlap,
  platform = as.character(session$platform),
  r_version = as.character(session$R.version$version.string),
  package_versions = package_versions,
  session_info = session
)

write_artifacts(
  results = results_df,
  metadata = metadata,
  outdir = outdir,
  run_id = run_id
)

log_line("benchmark run complete")
invisible(results_df)
