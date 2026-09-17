---
name: sas-macro-execution
description: Preserve program invocation, macro results, caller scope and repeated-call behavior.
metadata:
  sas2r:
    version: 2
    agents: [translator, reviewer, fixer]
    priority: 90
    triggers:
      flags: [macro_execution]
    tools: [get_macro_source, read_unit_context]
---

# Macro execution and returned values

Distinguish a reusable macro definition from an executable program. A standalone
macro component contains only its function assignment. A program that defines
and then invokes a macro must also execute its translated invocation, with the
source arguments, setup, conditions and loops. Defining the function alone is
not the program's behavior. Do not invoke every definition automatically or
invent calls for library-only files.

Trace each value from its producer to its consumer. When a macro calculates
values for its caller, prefer explicit returned values and assign them at the
call site. Preserve the declared function arguments and keep the returned
representation consistent with the callers. Do not assume that a source-only
interface describes an R return value: inspect the translated interface when
available, and report uncertainty when a dependency's return contract is unknown.

Use the source's local/global symbol-table behavior for genuinely shared state;
R lexical lookup and assignment to the global environment are not substitutes
for SAS's nested macro scopes. An old local value can shadow a newer global
value. Changing a loop to lapply() does not resolve captured or global state.

Assign each newly calculated result even when it is empty, if the source clears
or replaces it. Some source statements intentionally retain the old value on
empty input: preserve that behavior too. Do not reset every variable at every
iteration. Preserve deliberate accumulated state and pass source configuration
(including limits and pagination settings) to the actual consumer.

For a source helper that explicitly clears its result before each calculation,
this R pattern replaces a same-named old caller value on every call:

```r
calculate_limits <- function(x) {
  x <- x[!is.na(x)]
  list(limits = if (length(x)) range(x) else numeric())
}
build_panels <- function(groups, per_page = 2L) {
  limits <- c(-99, 99) # Earlier local binding: every calculation replaces it.
  panels <- list()    # Intentionally accumulated across iterations.
  for (i in seq_along(groups)) {
    limits <- calculate_limits(groups[[i]])$limits
    panels[[i]] <- list(limits = limits, page = (i - 1L) %/% per_page + 1L)
  }
  panels
}
```

Review a nonempty call followed by an empty call and then a different nonempty
call. Check that consumers see each intended value, that caller settings reach
the callee, and that accumulated state survives. Use this as a scope example,
not as a universal translation of all SAS assignments.

For a selected dependency documented to return a named count and blank label,
keep that shape explicit. This example also keeps a same-named caller value
separate from the callable function; it does not implement a macro symbol table.

```r
make_summary <- function(values) {
  list(count = c(total = length(values)), label = "", optional = NULL)
}
use_summary <- function(values) {
  make_summary_value <- "outer value"
  result <- make_summary(values)
  list(count = result$count[["total"]], label = result$label,
       has_optional_field = "optional" %in% names(result),
       outer_value = make_summary_value)
}
```

An absent field is different from a present blank field or a present NULL field.
Do not flatten this result into a guessed universal representation. This return
convention is justified only when both selected implementations use it.

Reference: [SAS macro variable scopes](https://support.sas.com/documentation/cdl/en/mcrolref/62978/HTML/default/p1b76sxg9dbcyrn1l5age5j5nvgw.htm).

## Quoting, defaults and changing loop inputs

SAS `%STR` can mask an unmatched parenthesis written as `%(` or `%)`.
For example `%str(%()` represents a literal opening parenthesis; counting all
parentheses without macro-quoting rules produces a false syntax allegation.
The offline parser is not a SAS compiler. Do not turn that allegation into an
intentional R error. Source: [SAS %STR and %NRSTR](https://support.sas.com/documentation/cdl/en/mcrolref/61885/HTML/default/a001061290.htm).

Use the provided source-owned defaults. Trigger-free text such as `75px`,
`two words` and `a+b` is not automatically an R expression. Dynamic expansion
needs context, and `NULL` is not a universal replacement for an omitted value.

Trace the value read by each next condition. In a loop that scans word `i` of
`tokens`, replaces `tokens` with `NONE` on an invalid word, then increments `i`,
the next condition sees the new string. For `bad 1`, iteration 2 sees no second
word; it does not visit the old trailing `1`. Snapshotting the old token vector
changes this source behavior. Explain a proposed reversal with a concrete trace.

## Dependencies and capability limits

Call the selected project function when the SAS delegates to that macro.
Preserve its actual return representation; do not guess Boolean versus 0/1.
Check the selected producer and caller together: a named scalar, a vector and
a named list have different interfaces. Extract the agreed field explicitly;
`unlist()` can change compound names and remove NULL fields. Blank text (`""`),
an absent binding and a missing argument are separate states. Canonicalize
identifier names consistently at both ends without changing their data values.
Do not invent an alias or a second global macro store to hide a mismatch.
Keep local shadowing, updates to existing outer values and explicit globals
distinct; never overwrite a callable function with a same-named macro value.
For numeric `x = c(NA, 1, 1)`, PROC SQL COUNT(DISTINCT x) is 1, not 2; empty input
has count 0. A caller that replaces a counting dependency with `length(unique(x))`
introduces a missing-value error. Character missing blanks also need exclusion.

Use `lib_exists(libref, member)` for registry data-member presence. A readable
zero-row table and a present unreadable file both exist. Do not implement EXIST
by catching every read/configuration error and returning false. Views and other
unsupported member types still need explicit handling.

A required dynamic WHERE expression cannot be silently dropped. Use existing
operations within documented scope; a parse/eval lint failure is not permission
to generate a general tokenizer/interpreter. Simple supported replacement remains
possible. Keep unsupported behavior visible, including when supplied call sites
exercise only an empty filter. Do not claim full reusable-macro support from that.

Numeric display does not justify changing the underlying value. Use documented
helper scope; field width is not a significant-digit count and an ad hoc R
formatter is not a complete SAS BEST implementation. A separate BEST helper
requires a supported-format design and independent fixtures; it is deferred.

These examples illustrate only the stated source operations, not a general macro interpreter:

```r
# PROC SQL select count(distinct x), for numeric x.
count_numeric_values <- function(x) length(unique(x[!is.na(x)]))
label_numeric_values <- function(x, count_fn = count_numeric_values) {
  list(count = count_fn(x)) # Preserve delegation and the numeric count.
}
# %do %while(%scan(&tokens,&i) ne ); mutate tokens on an invalid token,
# then increment i and test %scan of the CURRENT tokens again.
walk_current_words <- function(tokens) {
  i <- 1L
  visited <- character()
  repeat {
    words <- strsplit(trimws(tokens), " +")[[1L]]
    if (i > length(words) || !nzchar(words[i])) break
    word <- words[i]
    visited <- c(visited, word)
    if (is.na(suppressWarnings(as.numeric(word)))) tokens <- "NONE"
    i <- i + 1L
  }
  visited
}
```

Numeric and scope review examples
---------------------------------
- Keep a tiny nonzero interval such as `1e-8` nonzero in arithmetic. Formatting
  for display is not a reason to set it to zero. When source formatting feeds
  arithmetic, preserve that conversion at its source stage; do not move it
  after the calculation. General BEST parity remains unverified without a
  supported, independently justified format contract.
- A new ordinary scratch variable inside a macro is not automatically global.
  Distinguish updating an already existing outer binding from creating a local
  binding, and honor explicit `%LOCAL` shadowing and `%GLOBAL`. R `<<-` can
  modify an enclosing local environment; it does not always mean global.
- Keep a callable macro function separate from the value of a macro variable
  with the same name. Repeated calls must still reach the function.
- Give concrete SAS/R contradictions and small synthetic traces where useful.
  Missing proof of every numeric-format edge case is uncertainty, not an
  established mismatch; a visible wrong function or missing required effect
  can be established by static source/code evidence alone.
