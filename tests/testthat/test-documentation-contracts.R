# Documentation must not claim that sas2r automatically establishes parity with
# SAS. The requirement is claim *accuracy*, so match the shape of the claim --
# an affirmative claim verb applied to a parity/equivalence subject -- rather
# than a fixed list of phrasings that any near-synonym would slip past.
forbidden_parity_claims <- function(text) {
  spans <- unlist(strsplit(text, "(?<=[.!?;:])[[:space:]]+|\n+", perl = TRUE))
  spans <- spans[nzchar(trimws(spans))]
  claim <- paste0(
    "\\b(automatic|automatically|verif(y|ies|ied|ication|ications)|",
    "validat(e|es|ed|ion)|prove|proves|proven|proof|guarantee|guarantees|",
    "guaranteed|ensure|ensures|ensured|confirm|confirms|confirmed|",
    "certif(y|ies|ied)|demonstrat(e|es|ed)|attest(s|ed)?)\\b"
  )
  subject <- paste0(
    "\\b(parity|(sas|tlf|output|outputs|end.to.end)[ -]+equivalen(ce|t)|",
    "equivalen(ce|t)[ -]+(with|to|against)[ -]+sas)\\b"
  )
  clausal <- "^(not|never|no|nor|neither|cannot|can.t|isn.t|doesn.t|don.t|without|non)$"
  prepositional <- c("without", "non")
  boundary <- "(?i)[,;:]|\\b(and|but|or|nor|yet|while|whereas|although|though|however)\\b"

  span_is_disclaimed <- function(span) {
    hits <- gregexpr(subject, span, perl = TRUE, ignore.case = TRUE)[[1]]
    if (hits[1] < 0L) return(NA)
    for (start in hits) {
      before <- substr(span, 1L, start - 1L)
      clause <- if (grepl(paste0("(", boundary, ")\\s*$"), before, perl = TRUE)) {
        ""
      } else {
        segments <- strsplit(before, boundary, perl = TRUE)[[1]]
        if (length(segments)) segments[[length(segments)]] else ""
      }
      words <- tolower(unlist(regmatches(
        clause, gregexpr("[A-Za-z']+", clause))))
      negations <- which(grepl(clausal, words, perl = TRUE))
      if (!length(negations)) return(FALSE)
      reach <- ifelse(words[negations] %in% prepositional, 2L, 6L)
      if (!any(length(words) - negations <= reach)) return(FALSE)
    }
    TRUE
  }

  hit <- vapply(spans, function(span) {
    disclaimed <- span_is_disclaimed(span)
    !is.na(disclaimed) && !disclaimed &&
      grepl(claim, span, perl = TRUE, ignore.case = TRUE)
  }, logical(1), USE.NAMES = FALSE)
  spans[hit]
}

doc_r_chunks <- function(lines) {
  fence <- grepl("^\\s*```", lines)
  opens_r <- grepl("^\\s*```+\\s*(\\{\\s*[rR][ ,}]|[rR]\\s*$)", lines)
  chunks <- character()
  i <- 1L
  n <- length(lines)
  while (i <= n) {
    if (!fence[i]) {
      i <- i + 1L
      next
    }
    j <- i + 1L
    while (j <= n && !fence[j]) j <- j + 1L
    if (opens_r[i] && j > i + 1L) {
      chunks <- c(chunks, paste(lines[seq(i + 1L, j - 1L)], collapse = "\n"))
    }
    i <- j + 1L
  }
  chunks
}

doc_chunk_calls <- function(chunk) {
  parsed <- tryCatch(parse(text = chunk, keep.source = TRUE),
                     error = function(e) NULL)
  if (is.null(parsed)) return(character())
  pd <- utils::getParseData(parsed)
  if (is.null(pd) || !nrow(pd)) return(character())
  pd <- pd[pd$terminal, ]
  pd <- pd[order(pd$line1, pd$col1), ]
  calls <- which(pd$token == "SYMBOL_FUNCTION_CALL")
  qualified <- calls[calls > 1L & pd$token[pmax(calls - 1L, 1L)] %in%
                       c("NS_GET", "NS_GET_INT", "'$'", "'@'")]
  unique(pd$text[setdiff(calls, qualified)])
}

doc_prose_calls <- function(lines) {
  fence <- grepl("^\\s*```", lines)
  prose <- paste(lines[cumsum(fence) %% 2L == 0L & !fence], collapse = "\n")
  hits <- unlist(regmatches(
    prose, gregexpr("`[A-Za-z._][A-Za-z0-9._]*\\(\\)`", prose)))
  unique(gsub("[`()]", "", hits))
}

unexported_api_references <- function(lines) {
  ns <- asNamespace("sas2r")
  exported <- getNamespaceExports("sas2r")
  text <- paste(lines, collapse = "\n")

  internal <- unique(sub("^sas2r:::", "", unlist(regmatches(
    text, gregexpr("sas2r:::[A-Za-z._][A-Za-z0-9._]*", text)))))
  qualified <- unique(sub("^sas2r::", "", unlist(regmatches(
    text, gregexpr("sas2r::(?!:)[A-Za-z._][A-Za-z0-9._]*", text, perl = TRUE)))))

  # An explicit internal reference is honest, but it must still resolve.
  missing_internal <- Filter(
    function(name) !exists(name, envir = ns, inherits = FALSE), internal)
  # A `sas2r::` reference is a promise that NAMESPACE exports the symbol.
  unexported_qualified <- setdiff(qualified, exported)

  masked <- gsub("sas2r:::[A-Za-z._][A-Za-z0-9._]*", "internal_ref", lines)
  referenced <- unique(c(
    unlist(lapply(doc_r_chunks(masked), doc_chunk_calls)),
    doc_prose_calls(masked)))
  api_shaped <- Filter(function(name) {
    grepl("^sas_|^sas2r_", name) || exists(name, envir = ns, inherits = FALSE)
  }, referenced)

  deleted_symbols <- c(
    "sas_agent_translate", "sas_approve", "sas_assess",
    "sas_refine_with_outputs", "sas_review", "sas_review_queue",
    "sas_translate_all", "sas_write_report", "lint_r_code",
    "sas_cookbook_approve", "sas_cookbook_review", "sas_units"
  )
  helpers <- c(
    "lib_read", "lib_write", "sas_merge", "sas_sort", "chr_cmp",
    "sas_format", "sas_missing", "sas_truth", "sas_not_truth"
  )
  bare <- setdiff(setdiff(setdiff(api_shaped, exported), helpers), deleted_symbols)

  sort(unique(c(missing_internal, setdiff(unexported_qualified, deleted_symbols), bare)))
}

test_that("the doc symbol contract catches an unexported or missing API name", {
  expect_identical(
    unexported_api_references(c(
      "Use `sas_translate()` to translate a program.", "",
      "```r", "res <- sas_translate('a.sas')", "```")),
    character())
  expect_identical(
    unexported_api_references(c(
      "```r", "llm <- sas_attach_model(cfg)", "```")),
    "sas_attach_model")
  expect_identical(
    unexported_api_references("Skills are matched via `route_agent_skills()`."),
    "route_agent_skills")
  expect_identical(
    unexported_api_references("Matched via `sas2r:::route_agent_skills()`."),
    character())
  expect_identical(
    unexported_api_references("See `sas2r:::no_such_internal()`."),
    "no_such_internal")
  expect_identical(
    unexported_api_references("```r\nsas2r::ellmer_llm(cfg)\n```"),
    "ellmer_llm")
  expect_identical(
    unexported_api_references("Emitted via `sas_merge()`."),
    character())
})

test_that("every sas2r symbol shipped docs present as API is exported", {
  paths <- c(
    readme = test_path("..", "..", "README.md"),
    news = test_path("..", "..", "NEWS.md"),
    providers = test_path("..", "..", "docs", "llm-providers.md"),
    evidence = test_path("..", "..", "docs", "output-evidence.md"),
    migration_evidence = test_path("..", "..", "docs", "migration-evidence.md"),
    vignette = test_path("..", "..", "vignettes", "dependency-aware-migration.Rmd")
  )
  skip_if_not(all(file.exists(paths)), "source-tree documentation contract")

  for (nm in names(paths)) {
    offenders <- unexported_api_references(readLines(paths[[nm]], warn = FALSE))
    expect_identical(
      offenders, character(),
      info = paste0(nm, " names sas2r symbols that NAMESPACE does not export: ",
                    paste(offenders, collapse = ", ")))
  }
})

test_that("example config states exact ellmer timeout and retry semantics", {
  path <- system.file("examples", "_sas2r.example.yml", package = "sas2r")
  if (!nzchar(path) || !file.exists(path)) {
    path <- test_path("..", "..", "inst", "examples", "_sas2r.example.yml")
  }
  skip_if_not(file.exists(path), "packaged example config unavailable")
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(text, "absolute", fixed = TRUE)
  expect_match(text, "each HTTP transfer attempt", fixed = TRUE)
  expect_match(text, "defaults to 1", fixed = TRUE)
  expect_match(text, "does not interrupt an in-flight request", fixed = TRUE)
})

test_that("demo config does not promise ellmer tool/schema coexistence", {
  path <- system.file("examples", "demo_project", "_sas2r.yml", package = "sas2r")
  if (!nzchar(path) || !file.exists(path)) {
    path <- test_path("..", "..", "inst", "examples", "demo_project", "_sas2r.yml")
  }
  skip_if_not(file.exists(path), "packaged demo config unavailable")
  demo <- readLines(path, warn = FALSE)

  expect_false(any(grepl(
    "tools_with_structured_output: supported", demo, fixed = TRUE
  )))
  expect_true(any(grepl(
    "ellmer gathers with tools before structured finalization",
    demo, fixed = TRUE
  )))
})

test_that("forbidden-claim detector recognises the claim shape, not fixed phrasings", {
  offending <- c(
    "sas2r automatically verifies parity with your SAS outputs.",
    "sas2r does not require SAS and automatically verifies parity with your SAS outputs.",
    "No SAS licence is needed and sas2r guarantees SAS equivalence for every unit.",
    "Without extra configuration sas2r proves TLF equivalence end to end.",
    "sas2r proves TLF equivalence end to end.",
    "The package guarantees SAS equivalence for every translated unit.",
    "It confirms equivalence with SAS and needs no reviewer.",
    "Automatic output equivalence checking is built in."
  )
  for (s in offending) expect_length(forbidden_parity_claims(s), 1L)

  permitted <- c(
    "This is NOT parity.",
    "NOT VERIFIED AGAINST SAS OUTPUTS -- THIS IS NOT PARITY.",
    "Evaluates equivalence under SAS-aware tolerance rules.",
    "Applies collation semantics to verify content equivalence independently of row sequence.",
    "Double-programming verification on real data remains essential.",
    "It does not constitute mathematical proof of parity or grant automated approval.",
    "sas2r does not automatically verify parity with SAS.",
    "sas2r offers no proof of SAS equivalence."
  )
  for (s in permitted) expect_length(forbidden_parity_claims(s), 0L)
})

test_that("packaged beginner config has no mappings, keys, or tolerances", {
  path <- system.file("examples", "_sas2r.example.yml", package = "sas2r")
  if (!nzchar(path) || !file.exists(path)) {
    path <- test_path("..", "..", "inst", "examples", "_sas2r.example.yml")
  }
  skip_if_not(file.exists(path), "packaged example config unavailable")
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(text, "libraries:")
  expect_match(text, "macros:")
  expect_match(text, "includes:")
  expect_false(grepl("(?m)^[ \t]*(mappings|keys|tolerances):", text,
                     perl = TRUE, ignore.case = TRUE))
})

test_that("README documents dependency-aware migration workflow and contracts", {
  readme_path <- test_path("..", "..", "README.md")
  skip_if_not(file.exists(readme_path), "source-tree documentation contract")
  text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")

  # Core disclaimers
  expect_match(text, "does not require SAS", ignore.case = TRUE)
  expect_match(text, "NOT PARITY", fixed = TRUE)
  expect_false(grepl("sasr", text, ignore.case = TRUE))
  expect_identical(forbidden_parity_claims(text), character())

  # Single primary quick-start call and inspection
  expect_match(text, "sas_translate(", fixed = TRUE)
  expect_match(text, "max_program_repair_rounds = 1", fixed = TRUE)
  expect_match(text, "max_bundle_repair_rounds = 2", fixed = TRUE)
  expect_match(text, "result$status", fixed = TRUE)
  expect_match(text, "result$bundle_dir", fixed = TRUE)
  expect_match(text, "result$outputs_dir", fixed = TRUE)
  expect_match(text, "result$report_path", fixed = TRUE)
  expect_match(text, "sas_code(result, 1)", fixed = TRUE)

  # Four statuses and plain-language explanation
  expect_match(text, "blocked", fixed = TRUE)
  expect_match(text, "needs_review", fixed = TRUE)
  expect_match(text, "migration_ready", fixed = TRUE)
  expect_match(text, "validated", fixed = TRUE)

  # Key options and concepts
  expect_match(text, "execute", fixed = TRUE)
  expect_match(text, "outputs", fixed = TRUE)
  expect_match(text, "agent_evidence", fixed = TRUE)
  expect_match(text, "code_only", fixed = TRUE)
  expect_match(text, "copy-on-write", ignore.case = TRUE)
  expect_match(text, "directory", ignore.case = TRUE)
  expect_match(text, "sas_write", fixed = TRUE)

  # No obsolete workflow names or contracts
  expect_false(grepl("sas_approve", text, fixed = TRUE))
  expect_false(grepl("sas_assess", text, fixed = TRUE))
  expect_false(grepl("sas_refine_with_outputs", text, fixed = TRUE))
  expect_false(grepl("synth_agree", text, fixed = TRUE))
})

test_that("provider guide covers the closed registry and how to test it", {
  path <- test_path("..", "..", "docs", "llm-providers.md")
  skip_if_not(file.exists(path), "source-tree documentation contract")
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  ids <- c(
    "openai", "anthropic", "bedrock", "azure", "databricks", "deepseek",
    "github", "gemini", "vertex", "ollama", "posit", "snowflake"
  )
  expect_true(all(vapply(
    ids, function(id) grepl(paste0("`", id, "`"), text, fixed = TRUE),
    logical(1)
  )))
  expect_match(text, "sas_llm_models", fixed = TRUE)
  expect_match(text, "sas_llm_probe", fixed = TRUE)
  expect_match(text, "observe")
  expect_match(text, "Inf", fixed = TRUE)
  expect_match(text, "openai_compatible", fixed = TRUE)
})

test_that("output evidence guide covers seven concrete use cases and privacy boundary", {
  path <- test_path("..", "..", "docs", "output-evidence.md")
  skip_if_not(file.exists(path), "source-tree documentation contract")
  lines <- readLines(path, warn = FALSE)
  text <- paste(lines, collapse = "\n")

  # Seven use cases, numbered 1..7, in order.
  uc_idx <- grep("^### Use Case [0-9]+:", lines)
  expect_length(uc_idx, 7L)
  expect_identical(sub("^### Use Case ([0-9]+):.*$", "\\1", lines[uc_idx]),
                   as.character(1:7))

  # Every use case states its inputs, the work performed, and what is observable.
  heading_idx <- grep("^#{2,3} ", lines)
  for (i in uc_idx) {
    nxt <- heading_idx[heading_idx > i]
    end <- if (length(nxt)) nxt[1] - 1L else length(lines)
    block <- paste(lines[i:end], collapse = "\n")
    expect_match(block, "**Inputs**", fixed = TRUE)
    expect_match(block, "**Internal Work**", fixed = TRUE)
    expect_match(block, "**Observable Outputs**", fixed = TRUE)
  }

  # Six numbered top-level sections, in order.
  expect_identical(
    sub("^## ([0-9]+)\\..*$", "\\1", grep("^## [0-9]+\\.", lines, value = TRUE)),
    as.character(1:6)
  )

  expect_identical(forbidden_parity_claims(text), character())

  # 7 concrete use cases
  expect_match(text, "source only", ignore.case = TRUE)
  expect_match(text, "source plus LLM", ignore.case = TRUE)
  expect_match(text, "saved final outputs", ignore.case = TRUE)
  expect_match(text, "TLF", ignore.case = TRUE)
  expect_match(text, "missing", ignore.case = TRUE)
  expect_match(text, "duplicate", ignore.case = TRUE)

  # Data boundary & privacy
  expect_match(text, "local R process", ignore.case = TRUE)
  expect_match(text, "bounded", ignore.case = TRUE)
  expect_false(grepl("skills:", text, fixed = TRUE))
})

test_that("migration evidence guide documents evidence model, repair loops, and attempt contracts", {
  path <- test_path("..", "..", "docs", "migration-evidence.md")
  skip_if_not(file.exists(path), "source-tree documentation contract")
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")

  # Four component evidence levels
  expect_match(text, "reviewed_only", fixed = TRUE)
  expect_match(text, "runtime_verified", fixed = TRUE)
  expect_match(text, "output_verified", fixed = TRUE)
  expect_match(text, "reference_validated", fixed = TRUE)

  # Four bundle statuses
  expect_match(text, "blocked", fixed = TRUE)
  expect_match(text, "needs_review", fixed = TRUE)
  expect_match(text, "migration_ready", fixed = TRUE)
  expect_match(text, "validated", fixed = TRUE)

  # Reviewer outcomes
  expect_match(text, "reviewed_no_material_finding", fixed = TRUE)
  expect_match(text, "repair_required", fixed = TRUE)
  expect_match(text, "review_unavailable", fixed = TRUE)

  # Dual repair loops and attempts
  expect_match(text, "max_program_repair_rounds", fixed = TRUE)
  expect_match(text, "max_bundle_repair_rounds", fixed = TRUE)
  expect_match(text, "copy-on-write", ignore.case = TRUE)
  expect_match(text, "code_only", fixed = TRUE)

  expect_identical(forbidden_parity_claims(text), character())
})

test_that("vignette documents complete dependency-aware workflow, skills, and constraints", {
  path <- test_path("..", "..", "vignettes", "dependency-aware-migration.Rmd")
  skip_if_not(file.exists(path), "source-tree documentation contract")
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(text, "sas_translate", fixed = TRUE)
  expect_match(text, "migration_ready", fixed = TRUE)
  expect_match(text, "execute = FALSE", fixed = TRUE)
  expect_match(text, "resume", fixed = TRUE)
  expect_match(text, "sas_code", fixed = TRUE)
  expect_false(grepl("sasr", text, ignore.case = TRUE))
  expect_identical(forbidden_parity_claims(text), character())
})

test_that("public functions and package skills match code contracts", {
  ns <- asNamespace("sas2r")
  expect_true(exists("sas_translate", envir = ns, mode = "function"))
  expect_true(exists("sas_code", envir = ns, mode = "function"))
  expect_true(exists("sas_write", envir = ns, mode = "function"))
  expect_true(exists("sas_config", envir = ns, mode = "function"))

  skill_root <- system.file("skills", package = "sas2r")
  if (!nzchar(skill_root) || !dir.exists(skill_root)) {
    skill_root <- test_path("..", "..", "inst", "skills")
  }
  skills <- sas2r:::load_agent_skills(skill_root)
  expect_true(length(skills) >= 2L)
  skill_ids <- vapply(skills, `[[`, "", "skill_id")
  expect_true("sas-missing-sort-semantics" %in% skill_ids)
  expect_true("sas-dataset-row-alignment" %in% skill_ids)
})

model_boundary_disclosure_gaps <- function(text) {
  required <- c(
    row_numbers = "row number",
    identifiers = "identifier",
    key_values = "key value|\\bkeys\\b",
    cell_values = "cell value",
    residency = "residency"
  )
  present <- vapply(required, function(p) grepl(p, text, perl = TRUE, ignore.case = TRUE),
                    logical(1))
  names(required)[!present]
}

test_that("disclosure detector recognises a gap, not a phrasing", {
  complete <- paste(
    "Bounded reports may carry row numbers, subject identifiers, key values,",
    "and differing cell values. Confirm your endpoint meets your data",
    "residency obligations."
  )
  expect_identical(model_boundary_disclosure_gaps(complete), character())

  overclaim <- "All data comparison is SAS-free and confined to the local R process."
  expect_setequal(
    model_boundary_disclosure_gaps(overclaim),
    c("row_numbers", "identifiers", "key_values", "cell_values", "residency")
  )
})

test_that("every document describing the model boundary discloses what crosses it", {
  paths <- c(
    readme = test_path("..", "..", "README.md"),
    news = test_path("..", "..", "NEWS.md"),
    guide = test_path("..", "..", "docs", "output-evidence.md"),
    migration_guide = test_path("..", "..", "docs", "migration-evidence.md"),
    vignette = test_path("..", "..", "vignettes", "dependency-aware-migration.Rmd"),
    example = test_path("..", "..", "inst", "examples", "_sas2r.example.yml")
  )
  skip_if_not(all(file.exists(paths)), "source-tree documentation contract")

  for (nm in names(paths)) {
    text <- paste(readLines(paths[[nm]], warn = FALSE), collapse = "\n")
    expect_identical(
      model_boundary_disclosure_gaps(text), character(),
      info = paste("incomplete model-boundary disclosure in", nm)
    )
  }
})

test_that("output evidence guide documents caps, the truncation flag, and audit mode", {
  path <- test_path("..", "..", "docs", "output-evidence.md")
  skip_if_not(file.exists(path), "source-tree documentation contract")
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(text, "audit mode", ignore.case = TRUE)
  expect_match(text, "truncated", fixed = TRUE)
  expect_match(text, "serialized", ignore.case = TRUE)
  expect_match(text, "usage.jsonl", fixed = TRUE)
  expect_identical(forbidden_parity_claims(text), character())
})

test_that("provider table matches the registry and states an acceptance level", {
  path <- test_path("..", "..", "docs", "llm-providers.md")
  skip_if_not(file.exists(path), "source-tree documentation contract")
  lines <- readLines(path, warn = FALSE)

  header_i <- grep("^\\|\\s*Provider ID\\s*\\|", lines)
  expect_length(header_i, 1L)

  split_cells <- function(line) {
    trimws(strsplit(sub("\\|\\s*$", "", sub("^\\s*\\|", "", line)), "|", fixed = TRUE)[[1]])
  }
  header <- split_cells(lines[header_i])
  for (needed in c("Auth Modes", "Required", "Optional", "Acceptance")) {
    expect_true(any(grepl(needed, header, fixed = TRUE)),
                info = paste("provider table has no", needed, "column"))
  }

  body <- lines[seq(header_i + 2L, length(lines))]
  body <- body[seq_len(which(!grepl("^\\s*\\|", body))[1] - 1L)]
  rows <- lapply(body, split_cells)

  col <- function(name) which(grepl(name, header, fixed = TRUE))[1]
  ids <- vapply(rows, function(r) gsub("`", "", r[[1]]), character(1))
  registry <- sas2r:::llm_provider_registry()
  expect_setequal(ids, names(registry))

  backticked <- function(cell) {
    gsub("`", "", regmatches(cell, gregexpr("`[^`]+`", cell))[[1]])
  }

  for (i in seq_along(rows)) {
    row <- rows[[i]]
    spec <- registry[[ids[i]]]

    expect_match(row[[col("ellmer Constructor")]], spec$chat_export, fixed = TRUE)

    modes <- backticked(row[[col("Auth Modes")]])
    expect_identical(utils::head(modes, -1L), spec$auth_modes,
                     info = paste("auth modes for", ids[i]))
    expect_identical(utils::tail(modes, 1L), spec$default_auth_mode,
                     info = paste("default auth mode for", ids[i]))

    declared <- spec$config_fields
    listed <- c(backticked(row[[col("Required")]]), backticked(row[[col("Optional")]]))
    expect_true(all(declared %in% listed),
                info = paste("unplaced selectors for", ids[i], ":",
                             paste(setdiff(declared, listed), collapse = ", ")))

    expect_true(nzchar(row[[col("Acceptance")]]),
                info = paste("no acceptance level for", ids[i]))
  }

  required_cell <- function(id) rows[[which(ids == id)]][[col("Required")]]
  expect_true(all(c("endpoint", "api_version") %in% backticked(required_cell("azure"))))
  expect_true(all(c("project_id", "location") %in% backticked(required_cell("vertex"))))
  expect_true("base_url" %in% backticked(required_cell("ollama")))
  expect_true(all(c("region", "base_url") %in% backticked(required_cell("bedrock"))))
})
