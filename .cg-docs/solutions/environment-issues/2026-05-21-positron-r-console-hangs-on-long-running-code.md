---
date: 2026-05-21
title: "Positron R console hangs indefinitely on long-running code"
category: "environment-issues"
language: "R"
tags: [positron, r-console, microbenchmark, benchmarking, socket, jupyter, hang, freeze, executeCode]
root-cause: "Long-running R expressions sent through the Positron/Jupyter execution bridge block the socket and never return a response"
severity: "P1"
---

# Positron R Console Hangs on Long-Running Code

## Problem

When running expensive R code through the Positron interactive R console
(especially benchmarks using `microbenchmark` with large data and many
iterations), the session appears to freeze indefinitely — the spinner keeps
spinning and no output is returned, even after hours.

Symptoms:
- Positron shows "Working…" / loading spinner indefinitely
- No partial output is streamed
- R process is still alive (not crashed) but the IDE is blocked
- Restarting Positron is required to recover

## Root Cause

Positron routes R execution through a Positron supervisor ↔ Jupyter socket
bridge. The supervisor log shows:

```text
Unix socket connection ended (.../kc-5753.sock): error shutting down connection
```

If an R expression takes a long time (e.g. allocating 1M-row data.tables and
running `microbenchmark` with 20+ iterations), the socket layer times out or
closes before the response is ready. The IDE never receives a reply and appears
frozen, even though R is still computing.

Additionally, `microbenchmark` does not stream intermediate progress — it only
returns after **all** iterations are complete. This makes hang diagnosis
impossible without external process monitoring.

## Solution

**Do not run long-running benchmarks in the interactive R console.**

Use `Rscript` from the terminal instead, optionally redirecting output to a
log file:

```bash
Rscript inst/benchmarks/bench_joyn_microbenchmark.R \
  > inst/benchmarks/benchmark-run.log 2>&1
```

For checkpointed benchmarks that save results per size:

```r
# Save intermediate results so work is not lost if the session dies
saveRDS(result, file = paste0("inst/benchmarks/results/bench-", n, ".rds"))
gc()
```

For a "smoke test" before the full run:

```bash
JOYN_BENCH_SMOKE=true Rscript inst/benchmarks/bench_joyn_microbenchmark.R
```

## Prevention

- **Always run benchmarks via `Rscript` in terminal**, never interactively
  in the Positron R console.
- **Add progress `message()` calls** between benchmark blocks so terminal
  output confirms progress.
- **Save `.rds` checkpoints** after each size block so a hang or crash does
  not lose all work.
- **Start small**: run `times = 1` smoke tests before committing to full
  `times = 20` runs.
- Keep the largest sizes (`1M` rows) in a separate, explicitly acknowledged
  block with `times = 1` or `times = 2`.

## Related

- `inst/benchmarks/bench_joyn_microbenchmark.R` — the checkpointed benchmark
  runner that implements these practices
- See also: `2026-05-21-r-library-path-inconsistency-console-vs-terminal.md`
