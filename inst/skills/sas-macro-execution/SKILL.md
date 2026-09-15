---
name: sas-macro-execution
description: Preserve program invocation, macro results, caller scope and repeated-call behavior.
metadata:
  sas2r:
    version: 1
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

Reference: [SAS macro variable scopes](https://support.sas.com/documentation/cdl/en/mcrolref/62978/HTML/default/p1b76sxg9dbcyrn1l5age5j5nvgw.htm).
