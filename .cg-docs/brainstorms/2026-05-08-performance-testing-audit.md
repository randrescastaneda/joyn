---
date: 2026-05-08
title: "joyn Internal Performance Optimization, Testing, and Documentation Audit"
status: decided
scope: "Deep"
chosen-approach: "Targeted Internal Optimization + Benchmarks"
tags: [performance, testing, documentation, refactoring, benchmarks]
---

# joyn Internal Performance Optimization, Testing, and Documentation Audit

## Context

The team has noticed that joyn is slower than expected and has intermittent
failures that are hard to trace, particularly with complex joins involving
multiple key variables and type mismatches. The package is on CRAN with
external users, so the API (function signatures and parameter order) must
remain frozen. Internal refactoring is fair game.

## Requirements

1. **Performance**: Fix identified internal bottlenecks without changing user-facing API
2. **Reliability**: Harden fragile patterns (`sys.call(-1)`, `parent.frame()`, `grepl("keyby", ...)`)
3. **Testing**: Add missing tests for `prep_m_to_m_data.R`, type coercion edge cases, and performance benchmarks
4. **Documentation**: Fix typos in roxygen docs (minor scope — vignette rewrites deferred)
5. **Dependencies**: Drop `glue` in favor of `cli::format_inline()` (approved)
6. **Constraint**: `collapse` dependency stays — it's the fastest join engine available
7. **Out of scope this iteration**: New join types, new exported functions, vignette rewrites, CRAN submission

## Approaches Considered

### Approach 1: Targeted Internal Optimization (Chosen)

- Batch messages: pre-allocate list, convert to data.frame once
- Collapse double `ftransform` in workhorse to single operation
- Simplify `check_var_class`: use `class()` against a set instead of 8 `inherits()` calls
- Drop `glue` dependency → `cli::format_inline()`
- Harden `clear_joynenv()`: replace `sys.call(-1)` with explicit caller argument
- Add missing tests for `prep_m_to_m_data.R` and type coercion edge cases
- Fix documentation typos

**Pros**: Low risk, backward-compatible, addresses real pain points, incremental
**Cons**: Doesn't address the fundamental "full join then filter" design
**Effort**: Medium

### Approach 2: Architecture-Level Join Optimization

- Modify `joyn_workhorse()` to accept `how` parameter, dispatch targeted joins
- Only fall back to full join when diagnostics require it

**Pros**: Significant speed gain for non-full joins
**Cons**: High risk — could break reporting semantics silently
**Effort**: Large
**Not chosen**: The diagnostics rely on the full join; changing this could break reporting

### Approach 3: Comprehensive Benchmark Suite

- Build benchmarks profiling `joyn()` across dataset sizes and join types
- Use results to validate improvements

**Pros**: Data-driven prioritization
**Cons**: Delays improvements if done standalone
**Not chosen standalone**: Integrated into Approach 1 as validation

## Decision

**Approach 1 + benchmarking from Approach 3.** Fix known bottlenecks one-by-one,
validate each improvement with benchmarks, and add comprehensive tests for
every change plus missing coverage areas.

## Next Steps

1. Create benchmark suite (baseline before any changes)
2. Batch messaging system optimization
3. Workhorse `ftransform` consolidation
4. `check_var_class` simplification
5. Drop `glue` dependency
6. Harden `clear_joynenv()` and other fragile patterns
7. Add missing tests (`prep_m_to_m_data.R`, type coercion, edge cases)
8. Fix documentation typos
9. Run benchmarks again to measure gains
