# ADR 0003: How the runtime helpers are distributed

**Status:** accepted; phases 1 and 2 implemented · **Date:** 2026-09-07 · **Affects:** `inst/templates/sas2r-helpers.R`,
`write_helpers()`, package documentation, and a future `sas2r.runtime` package.

## Context

Every program sas2r translates calls a small runtime — `lib_read()`, `lib_write()`,
`sas_sort()`, `sas_merge()`, `chr_cmp()`, the format machinery and the rest, thirty functions
in all. That runtime is not part of the sas2r package namespace. It lives in
`inst/templates/sas2r-helpers.R` and is copied verbatim into every generated bundle, which the
bundle's programs `source()` through their bootstrap header. This keeps the README's promise:
translated programs run *without sas2r installed*.

Two costs have surfaced as the package reaches users:

1. **Discoverability.** `?lib_read` finds nothing; `help(package = "sas2r")` lists only the
   sixteen exported functions. The code users read most calls thirty functions R knows nothing
   about, and inside the helper file fewer than a third carried a comment.
2. **Drift and validation.** Each bundle carries an unversioned, frozen copy. A fix to a helper
   reaches no existing bundle, the copies in the wild silently diverge, and a regulated user has
   to treat every bundle's helper *file* as something to validate, rather than validating a
   runtime once.

As adoption grows these helpers become a de facto standard API for SAS-faithful semantics in R,
and an unversioned, undocumented standard is a liability.

## Decision

**A hybrid: one source of truth, vendored by default.**

1. The runtime becomes documented, versioned, exported package code, so it has a help page, an
   API contract, and a place for the community to contribute fixes.
2. The generator keeps writing a **version-stamped** copy of the runtime into every bundle.
   Bundles stay standalone and frozen: a bundle's behavior depends only on its own contents.
3. A bundle never prefers an installed runtime over its vendored copy. The installed form exists
   for documentation, interactive use, and organizational validation — not to change what a
   bundle does.

This mirrors the split the pharma R ecosystem already converged on (a runtime library such as
`admiral` alongside the tooling that generates code for it): organizations can validate the
runtime once, the way they validate `admiral`, instead of validating a helper file per bundle.

### Phases

| Phase | What lands | Bundles |
|---|---|---|
| **1 — document** (this ADR's PR) | `?sas2r_runtime` help topic (with an alias per helper, so `?lib_read` resolves once sas2r is attached), `vignette("runtime-helpers")`, header comments in the helper file, a version stamp written by `write_helpers()`, contract tests keeping docs and template in sync, this policy. | unchanged |
| **2 — one source, before 1.0** (done) | Helper source lives in `R/runtime-*.R`: 21 helpers exported with a help page and runnable examples each, the two operators and six plumbing helpers kept internal (exporting `%+%` would mask ggplot2's). `inst/templates/sas2r-helpers.R` is rendered from those files by `tools/build-runtime-template.R`; tests fail if the template is stale or if any vendored function differs from the package's. Registry lookup is the one context-dependent piece and now goes through `sas2r_registry_env()` -- lexically where the runtime was loaded, then the global environment, then any active frame -- so the same code serves both forms. Naming settled: `sas_*` for SAS semantics, `sas2r_*` for runtime plumbing, `lib_*` for data access. The template's `emit_proc_sort()` alias is gone: it collided with the package's PROC SORT *emitter* of the same name once both lived in one namespace, and no generated code ever called it (bundles from earlier versions keep their own frozen copy). | unchanged in form; the vendored file is a snapshot of a known release |
| **3 — separate package, when usage justifies it** | `sas2r.runtime` on CRAN; sas2r imports it; an opt-in `runtime = "package"` mode writes `library(sas2r.runtime)` into bundles for organizations that validate the runtime once. | vendored by default; package mode opt-in |

### Naming

The future package is **`sas2r.runtime`**. It is free on CRAN (no package name begins with
`sas2r`), dotted names have long precedent there (`data.table`, `future.apply`), the parent
package is visible in the name, and "runtime" says exactly what it is. The concatenated
pharmaverse style (`sas2rruntime`) is less readable; a short generic name (`sasr`) invites
collision and says nothing. Function names inside the runtime keep their current, unprefixed
forms (`lib_read`, `sas_sort`): the package name groups them, it does not rename them, so the
split is invisible to bundles.

### Stability policy (in force from phase 1)

- Runtime versions follow the sas2r version until phase 3, then their own semantic version.
  The vendored file states which version generated it.
- An existing helper never changes semantics within a major version. A change in what a helper
  does, for the same inputs, is a major version.
- A helper is retired only through a deprecation period: a replacement ships first, the old
  name warns for at least one minor version, then it is removed.
- The lint allowlist (`SAS2R_HELPER_NAMES`), the documentation topic, and the template are held
  in sync by tests; a helper cannot be added or removed in one place only.

## Consequences

- Users get `?lib_read`, a vignette, and a stated stability promise now, with no change to what
  a bundle contains beyond a header comment.
- Phase 2 is where the real cost sits: moving thirty functions into `R/`, generating the template,
  and the naming tidy-up. It is deliberately scheduled before 1.0, while the API can still move.
- Phase 3 adds a second CRAN package to maintain. It is gated on demand, not on the calendar.
- Rejected: exporting the helpers from sas2r *and* keeping a hand-maintained template copy (two
  implementations that will drift), and making bundles `library()` the runtime by default
  (breaks the standalone promise and lets an upgrade change validated behavior).
