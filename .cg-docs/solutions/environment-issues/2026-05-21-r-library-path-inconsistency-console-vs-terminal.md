---
date: 2026-05-21
title: "R library path inconsistency between Positron console and terminal Rscript"
category: "environment-issues"
language: "R"
tags: [positron, r-console, library-path, libPaths, Renviron, R_LIBS_USER, microbenchmark, package-not-found]
root-cause: "~/.Renviron was missing, so Positron's R console did not include the user library in .libPaths() even though R_LIBS_USER was set"
severity: "P2"
---

# R Library Path Inconsistency Between Positron Console and Terminal

## Problem

A package (`microbenchmark`) was successfully installed and usable from
terminal `Rscript`, but `requireNamespace("microbenchmark", quietly = TRUE)`
returned `FALSE` in the Positron R console.

Running `.libPaths()` in the console returned only:

```r
"/Library/Frameworks/R.framework/Versions/4.6/Resources/library"
```

While terminal `Rscript` returned:

```r
"/Users/acastanedaa/Library/R/arm64/4.6/library"
"/Library/Frameworks/R.framework/Versions/4.6/Resources/library"
```

## Root Cause

`~/.Renviron` did not exist.

Without it, `R_LIBS_USER` is set by R's default startup logic at lower
priority. In some Positron configurations, the console R session does not
pick up `R_LIBS_USER` from the macOS environment the same way a terminal
shell does. The terminal inherits the full shell environment, but
Positron's GUI-launched R process may not.

Diagnosis:

```r
# In Positron console
.libPaths()
#> "/Library/Frameworks/R.framework/Versions/4.6/Resources/library"

Sys.getenv("R_LIBS_USER")
#> "/Users/acastanedaa/Library/R/arm64/4.6/library"

# env var is set but .libPaths() does not include it -> ~/.Renviron is missing
```

## Solution

Create `~/.Renviron` to explicitly declare `R_LIBS_USER`. This is loaded by
**all** R sessions (console, Rscript, Positron, RStudio) before `.libPaths()`
is assembled.

```bash
# Run once in terminal
mkdir -p "$HOME/Library/R/arm64/4.6/library"
printf 'R_LIBS_USER=%s\n' "$HOME/Library/R/arm64/4.6/library" > "$HOME/.Renviron"
cat "$HOME/.Renviron"
```

Expected `~/.Renviron` content:

```text
R_LIBS_USER=/Users/acastanedaa/Library/R/arm64/4.6/library
```

After creating, **fully restart Positron**.

Verification:

```r
.libPaths()
#> "/Users/acastanedaa/Library/R/arm64/4.6/library"
#> "/Library/Frameworks/R.framework/Versions/4.6/Resources/library"

requireNamespace("microbenchmark", quietly = TRUE)
#> TRUE
```

## Prevention

- **Always create `~/.Renviron`** when setting up a new macOS R environment.
- **Do not rely on `R_LIBS_USER` being inherited from the shell** — unreliable
  for GUI-launched R sessions (Positron, RStudio).
- Use `~/.Renviron` (not `.Rprofile`) for library path configuration — it is
  applied earlier in the startup sequence and is more reliable.
- **Keep one user library per R version**:
  ```text
  ~/Library/R/arm64/<R-version>/library
  ```
  Do not share a single library across R versions — compiled packages will
  break on version upgrades.
- After every major R version upgrade (e.g. 4.6 -> 4.7), update `~/.Renviron`
  to point at the new versioned path and reinstall user packages.

## Related

- `2026-05-21-positron-r-console-hangs-on-long-running-code.md` — related
  session environment issue
- R documentation: `?Startup` (startup file load order)
- `usethis::edit_r_environ()` — opens `~/.Renviron` for editing
