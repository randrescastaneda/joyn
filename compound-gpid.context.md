# Project Context

Additional context for Copilot and the Compound GPID plugin. Edit freely —
this file is committed to git and shared with the team.

## Data Sources
<!-- Where does data come from? File paths, databases, APIs, vintage conventions -->

## Domain Rules

- **Never run long-running benchmarks in the Positron interactive R console.**
  Use `Rscript` in the terminal only. The Positron R console execution bridge
  (`kcserver` → Jupyter socket) will hang silently when R does not return
  within a reasonable time window. This applies to `microbenchmark`,
  `bench::mark()`, and any other blocking call expected to run > ~10 s.
  Always redirect output to a log file:
  ```bash
  Rscript inst/benchmarks/bench_joyn_microbenchmark.R \
    > inst/benchmarks/bench_joyn_microbenchmark.log 2>&1
  ```

- **`~/.Renviron` must exist with `R_LIBS_USER` set** to ensure consistent
  package resolution across the Positron console and the terminal:
  ```text
  R_LIBS_USER=/Users/acastanedaa/Library/R/arm64/4.6/library
  ```
  Without this, packages installed via `install.packages()` in the terminal
  land in the user library but are invisible to the Positron R console.
  Restart Positron after creating or editing `~/.Renviron`.

## Work in Progress

- `refactor/performance-testing-docs-audit` branch — performance audit,
  benchmarking infrastructure (`inst/benchmarks/`), and docstring corrections.
- Checkpointed `microbenchmark` runner:
  `inst/benchmarks/bench_joyn_microbenchmark.R`
  Full run (10K → 100K → 500K → 1M) not yet executed — must be run via
  terminal `Rscript`, not the interactive console.

## Workspace Notes
<!-- Related folders, dependencies on other projects in the VS Code workspace -->
