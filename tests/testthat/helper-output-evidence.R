# Deterministic, no-network test fixture constructors for output evidence

output_lineage_fixture <- function(code = c("data work.stage; set sdtm.dm; run;",
                                            "data adam.adsl; set work.stage; run;"),
                                   program = "adsl.sas",
                                   libraries = list(adam = "data/adam", sdtm = "data/sdtm", tlfdata = "data/tlfdata")) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  for (nm in names(libraries)) {
    dir.create(file.path(dir, libraries[[nm]]), recursive = TRUE, showWarnings = FALSE)
  }
  prog_path <- file.path(dir, program)
  dir.create(dirname(prog_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(code, prog_path)

  lib_lines <- paste0("  ", names(libraries), ": ", unlist(libraries))
  r_lib_lines <- paste0("      ", names(libraries), ": ", unlist(libraries))
  writeLines(c(
    "libraries:",
    lib_lines,
    "verification:",
    "  output_review:",
    "    enabled: true",
    "    r_libraries:",
    r_lib_lines
  ), file.path(dir, "_sas2r.yml"))

  sas_project(dir)
}

output_inventory_fixture <- function(reference = character(),
                                     candidate = character(),
                                     data = data.frame(id = 1:5, x = letters[1:5])) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  ref_dir <- file.path(dir, "ref")
  cand_dir <- file.path(dir, "cand")
  dir.create(ref_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cand_dir, recursive = TRUE, showWarnings = FALSE)

  # Write reference files
  for (r in reference) {
    target_path <- file.path(ref_dir, r)
    dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
    ext <- tolower(tools::file_ext(r))
    if (ext == "sas7bdat") {
      if (ext == "xpt") {
        haven::write_xpt(data, target_path)
      } else {
        writeBin(as.raw(c(0x00, 0x01, 0x02, 0x03)), target_path)
      }
    } else if (ext == "xpt") {
      haven::write_xpt(data, target_path)
    } else if (ext == "rds") {
      saveRDS(data, target_path)
    }
  }

  # Write candidate files
  for (c in candidate) {
    target_path <- file.path(cand_dir, c)
    dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
    ext <- tolower(tools::file_ext(c))
    if (ext == "rds") {
      saveRDS(data, target_path)
    } else if (ext == "xpt") {
      haven::write_xpt(data, target_path)
    } else {
      writeLines("test", target_path)
    }
  }

  cfg <- structure(list(
    libraries = list(adam = list(path = normalizePath(ref_dir, winslash = "/", mustWork = FALSE),
                                 engine = "sas7bdat", write = "rds")),
    output_review = list(
      enabled = TRUE,
      r_libraries = list(adam = normalizePath(cand_dir, winslash = "/", mustWork = FALSE))
    ),
    macro_search_path = character(),
    include_roots = character(),
    autoexec = character(),
    llm = NULL,
    budget = list(),
    source = file.path(dir, "_sas2r.yml"),
    raw = list()
  ), class = "sas2r_config")

  build_output_inventory(NULL, cfg)
}

output_target_fixture <- function(target_id = "target_0001",
                                  logical_dataset = "adam.adsl",
                                  role = "final_dataset",
                                  program = "adsl.sas",
                                  consumer_proc = NA_character_,
                                  unit_ids = 1L) {
  tibble::tibble(
    target_id = target_id,
    logical_dataset = logical_dataset,
    role = role,
    program = program,
    consumer_proc = consumer_proc,
    contributing_unit_ids = list(as.integer(unit_ids)),
    reference_candidate_id = "candidate_000001",
    r_candidate_id = "candidate_000002",
    reference_options = list(character()),
    r_options = list(character()),
    pairing_method = "exact_logical_name",
    status = "paired",
    reason = NA_character_
  )
}

alignment_context <- function(by = character(),
                              sort = list(vars = by, descending = rep(FALSE, length(by))),
                              merge = list(),
                              known_identifiers = by,
                              order_contract = NULL) {
  list(
    by = as.character(by),
    sort = sort,
    merge = merge,
    known_identifiers = as.character(known_identifiers),
    order_contract = order_contract
  )
}

empty_alignment_context <- function() {
  list(
    by = character(),
    sort = list(vars = character(), descending = logical()),
    merge = list(),
    known_identifiers = character(),
    order_contract = NULL
  )
}

comparison_report_registry_fixture <- function() {
  env <- new.env(parent = emptyenv())
  rep <- list(
    schema_version = sas2r:::OUTPUT_REPORT_SCHEMA_VERSION,
    report_id = "report-1",
    target_id = "target-1",
    logical_dataset = "adam.adsl",
    role = "final_dataset",
    contributing_unit_ids = 1L,
    staged_code_hashes = "abc",
    structure = list(),
    alignment = list(),
    order = list(meaningful = FALSE),
    mismatches = list(),
    examples = tibble::tibble(
      reference_row = integer(),
      candidate_row = integer(),
      key_values = character(),
      variable = character(),
      reference_value = character(),
      candidate_value = character()
    ),
    resource_state = "complete",
    truncated = FALSE,
    truncated_fields = character(),
    stale = FALSE,
    stale_reason = NA_character_,
    reference_hash = "refhash",
    candidate_hash = "candhash",
    profile_hash = "profhash",
    schema_hash = "schhash"
  )
  class(rep) <- "sas2r_comparison_report"
  env[["report-1"]] <- rep
  env
}

ambiguous_target_plan_fixture <- function(options = c("cand-a", "cand-b"),
                                          reference_options = "candidate_000001",
                                          aliases = character()) {
  # A real target plan carries a resolver over real, distinct, confined files.
  # Without one an agent-chosen pair cannot clear the confinement,
  # same-physical-file, and hash-freshness guards that pairing requires.
  #
  # `reference_options` and `options` may overlap, and `aliases` -- a named
  # vector of new_id = existing_id -- registers a second opaque ID over an
  # existing physical file. Both exist so a model choice can actually land both
  # sides of a pair on one file: with disjoint option sets over distinct files
  # the same-physical-file guard is unreachable, and a test of it would pass
  # against an implementation that never applied it.
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  resolver <- new.env(parent = emptyenv())
  root <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  reg <- function(cid, file_stem = cid) {
    f <- file.path(dir, paste0(file_stem, ".rds"))
    if (!file.exists(f)) saveRDS(data.frame(id = 1L), f)
    canon <- normalizePath(f, winslash = "/", mustWork = TRUE)
    resolver[[cid]] <- list(
      candidate_id = cid, side = "candidate", root_id = "root_0001",
      libref = "out", relative_name = basename(f), logical_name = "adam.adsl",
      format = "rds", size_bytes = file.size(f),
      file_hash = as.character(cli::hash_file_sha256(canon)),
      status = "available", reason = NA_character_,
      path = canon, root = root
    )
  }
  owned <- setdiff(unique(c("candidate_000001", reference_options, options)),
                   names(aliases))
  for (cid in owned) reg(cid)
  for (cid in names(aliases)) reg(cid, file_stem = aliases[[cid]])

  structure(
    list(
      resolver = resolver,
      plan = tibble::tibble(
        target_id = "target_0001",
        logical_dataset = "adam.adsl",
        role = "final_dataset",
        program = "adsl.sas",
        consumer_proc = NA_character_,
        contributing_unit_ids = list(1L),
        reference_candidate_id = reference_options[1],
        r_candidate_id = NA_character_,
        reference_options = list(reference_options),
        r_options = list(options),
        pairing_method = "ambiguous",
        status = "ambiguous",
        reason = "multiple_candidate_options"
      ),
      options = options,
      # The fixture's own temp root. Tests that assert nothing resembling a
      # filesystem path reaches the model need the needle to search for.
      root = root,
      dir = dir
    ),
    class = "sas2r_output_target_plan"
  )
}

output_agent_fixture <- function(contributing = 7L, frozen = integer()) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  prog <- file.path(dir, "adsl.sas")
  writeLines("data adam.adsl; set sdtm.dm; run;", prog)
  # Relative to out_dir, exactly as sas_transpile() records it. An absolute
  # path here would hide a resolver that ignores out_dir.
  staged_rel <- "adsl.R"
  staged_file <- file.path(dir, staged_rel)
  # Marker-delimited, as every staged file transpile writes actually is --
  # a marker-less file would make splice_unit_code() abort.
  writeLines(c(
    sprintf("# --- sas2r:unit %d ---", as.integer(contributing)[1]),
    "adam_adsl <- sdtm_dm",
    sprintf("# sas2r:end unit=%d", as.integer(contributing)[1])
  ), staged_file)

  manifest <- tibble::tibble(
    unit_id = as.integer(contributing),
    staged_file = staged_rel,
    tier = "t1",
    flags = ifelse(as.integer(contributing) %in% frozen, "frozen", "")
  )

  tr <- list(
    out_dir = dir,
    manifest = manifest,
    project = list(dir = dir)
  )

  report <- list(
    schema_version = sas2r:::OUTPUT_REPORT_SCHEMA_VERSION,
    report_id = "report_0001",
    target_id = "target_0001",
    logical_dataset = "adam.adsl",
    role = "final_dataset",
    contributing_unit_ids = as.integer(contributing),
    staged_code_hashes = "abc",
    structure = list(),
    alignment = list(),
    order = list(meaningful = FALSE),
    mismatches = list(),
    examples = tibble::tibble(
      reference_row = 1L,
      candidate_row = 1L,
      key_values = "01",
      variable = "aval",
      reference_value = "10",
      candidate_value = "20"
    ),
    resource_state = "complete",
    truncated = FALSE,
    truncated_fields = character(),
    stale = FALSE,
    stale_reason = NA_character_,
    reference_hash = "h1",
    candidate_hash = "h2",
    profile_hash = "h3",
    schema_hash = "h4"
  )
  class(report) <- "sas2r_comparison_report"

  list(
    report = report,
    translation = tr,
    llm = new_llm(function(req) structure(list(status = "completed", action = "final", data = list()), class = "sas2r_llm_response"), provider = "mock"),
    budget = new_usage_budget()
  )
}

# Every character leaf of an outbound request: prompt text, tool descriptions,
# tool argument schemas, model and schema names. Functions and environments
# are skipped because they never travel to the provider.
outbound_request_strings <- function(x) {
  if (is.function(x) || is.environment(x)) return(character())
  if (is.character(x)) return(as.character(x))
  if (is.list(x)) {
    parts <- unlist(lapply(x, outbound_request_strings), use.names = FALSE)
    return(if (is.null(parts)) character() else as.character(parts))
  }
  character()
}

# A trace only an in-process evaluation of model-authored R can leave. The
# counter lives in this session's memory, so the sandboxed synthetic execution
# the verification gates perform in a callr subprocess cannot reach it and
# cannot produce a false positive: a nonzero count means this session itself
# ran the model's code.
local_exec_sentinel <- function(name = "sas2r_exec_sentinel_marker",
                                .local_envir = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$hits <- 0L
  if (exists(name, envir = globalenv(), inherits = FALSE)) {
    stop("exec sentinel name '", name, "' is already bound in globalenv()")
  }
  assign(name, function() {
    state$hits <- state$hits + 1L
    TRUE
  }, envir = globalenv())
  withr::defer(rm(list = name, envir = globalenv()), envir = .local_envir)
  list(
    # Parses, lints clean (an unknown function is `info`, not `error`), and
    # calls the sentinel the moment anything evaluates it.
    code = paste0("adam_adsl <- ", name, "()"),
    hits = function() state$hits
  )
}

test_usage_budget <- function(mode = "observe", max_calls = 100L) {
  new_usage_budget(mode = mode, max_calls = max_calls)
}

staged_file_hashes <- function(dir) {
  # Ask for the relative names directly rather than slicing the absolute ones.
  # Slicing mixed a normalized prefix length against unnormalized paths, and on
  # macOS -- where normalizePath() prepends "/private" -- that cut eight extra
  # characters off every name, yielding "" and "elpers.R" instead of the real
  # relative paths. Names identify which file moved, so wrong names make an
  # expect_identical() failure unreadable.
  rel <- list.files(dir, pattern = "\\.R$", recursive = TRUE)
  if (!length(rel)) return(character())
  hashes <- vapply(file.path(dir, rel),
                   function(f) as.character(cli::hash_file_sha256(f)),
                   character(1), USE.NAMES = FALSE)
  stats::setNames(hashes, rel)
}

output_contract_fixture_project <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(dir, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "data/sdtm"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "reports"), recursive = TRUE, showWarnings = FALSE)

  code <- c(
    "data work.stage;",
    "  set sdtm.dm;",
    "run;",
    "",
    "data adam.adsl;",
    "  set work.stage;",
    "run;",
    "",
    "ods pdf file=\"reports/t14-1.pdf\";",
    "proc report data=adam.adsl;",
    "  columns usubjid trt;",
    "run;",
    "ods pdf close;",
    "",
    "ods rtf file=\"reports/l14-2.rtf\";",
    "proc print data=adam.adsl;",
    "run;",
    "ods rtf close;",
    "",
    "ods listing gpath=\"reports\";",
    "ods graphics on / reset imagename=\"f14-3\" imagefmt=png outdir=\"reports\";",
    "proc sgplot data=adam.adsl;",
    "  vbar trt;",
    "run;",
    "ods listing close;"
  )
  writeLines(code, file.path(dir, "report.sas"))

  writeLines(c(
    "libraries:",
    "  adam: data/adam",
    "  sdtm: data/sdtm"
  ), file.path(dir, "_sas2r.yml"))

  sas_project(dir)
}

dynamic_tlf_fixture_project <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(dir, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "data/sdtm"), recursive = TRUE, showWarnings = FALSE)

  code <- c(
    "data adam.adsl;",
    "  set sdtm.dm;",
    "run;",
    "",
    "ods pdf file=\"&outdir/t14-1.pdf\";",
    "proc report data=adam.adsl;",
    "  columns usubjid;",
    "run;",
    "ods pdf close;"
  )
  writeLines(code, file.path(dir, "dynamic_report.sas"))

  writeLines(c(
    "libraries:",
    "  adam: data/adam",
    "  sdtm: data/sdtm"
  ), file.path(dir, "_sas2r.yml"))

  sas_project(dir)
}

