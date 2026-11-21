render_unit_comment_evidence <- function(project, unit_id) {
  comments <- project$comments
  required <- c("unit_id", "char_start", "file", "line_start", "line_end",
                "kind", "placement", "text")
  if (is.null(comments) || !is.data.frame(comments) || !nrow(comments) ||
      !all(required %in% names(comments))) {
    return("(none attached)")
  }
  attached <- comments[!is.na(comments$unit_id) &
                         comments$unit_id == unit_id, , drop = FALSE]
  if (!nrow(attached)) return("(none attached)")
  attached <- attached[order(attached$char_start), , drop = FALSE]
  paste(sprintf(
    "[%s; lines %d-%d; %s; %s]\n%s",
    attached$file, attached$line_start, attached$line_end,
    attached$kind, attached$placement, attached$text
  ), collapse = "\n\n")
}

#' Emit generated macro R function file and testthat spec
#' @param result List with the translated `code` and its `macro_contract`.
#' @param macro_name Name of the macro.
#' @param out_dir Output project directory.
#' @param is_fallback Logical indicating if macro_name was auto-generated fallback.
#' @return Invisible list containing output directory `dir`, logical `is_fallback`, and character `macro_name`.
#' @noRd
emit_macro_artifacts <- function(result, macro_name, out_dir, is_fallback = FALSE) {
  if (is.null(macro_name) || is.na(macro_name) || !nzchar(macro_name)) {
    cli::cli_abort("macro name is missing; refusing to write artifacts",
                   class = "sas2r_macro_name_error")
  }
  safe_name <- gsub("[^A-Za-z0-9_]", "_", macro_name)
  if (!nzchar(safe_name) || identical(safe_name, "unnamed_macro")) {
    safe_name <- "unnamed_macro"
    is_fallback <- TRUE
  }
  fn_dir <- file.path(out_dir, "R", "macros")
  dir.create(fn_dir, showWarnings = FALSE, recursive = TRUE)
  origin <- if (isFALSE(result$llm_authored)) "macro_deferred" else "llm_authored"
  writeLines(c(sprintf("# sas2r:%s macro=%s -- REVIEW REQUIRED", origin, safe_name),
               "# semantics unverified in v1 (no macro expander); see manifest flags",
               result$code),
             file.path(fn_dir, paste0(safe_name, ".R")))
  if (!is_fallback) {
    ts_dir <- file.path(out_dir, "tests_macros")
    dir.create(ts_dir, showWarnings = FALSE, recursive = TRUE)
    string_literal <- function(value) paste(deparse(value), collapse = "")
    test_lines <- c(
      sprintf('test_that("%s is a callable function with stable formals", {', safe_name),
      sprintf('  candidates <- file.path(c("macros", "../macros", "R/macros", "../R/macros"), "%s.R")', safe_name),
      '  fn_path <- candidates[file.exists(candidates)][1L]',
      '  source(fn_path, local = TRUE)',
      sprintf("  fn <- get(%s, inherits = FALSE)", string_literal(safe_name)),
      '  expect_true(is.function(fn))',
      if (!is.null(result$macro_contract)) {
        parameters <- result$macro_contract$parameters
        expected_names <- paste(vapply(parameters$name, string_literal, character(1)),
                                collapse = ", ")
        c(
          sprintf("  expect_identical(names(formals(fn)), c(%s))", expected_names),
          vapply(which(parameters$default_status == "known"), function(i) {
            value <- parameters$r_default[[i]]
            expected <- string_literal(value)
            if (is.numeric(value) && length(value) == 1L && !is.na(value)) {
              return(sprintf(
                "  expect_identical(eval(formals(fn)[[%s]]), %s)",
                string_literal(parameters$name[i]), expected
              ))
            }
            sprintf(
              "  expect_identical(formals(fn)[[%s]], %s)",
              string_literal(parameters$name[i]), expected
            )
          }, character(1))
        )
      },
      "})")
    writeLines(test_lines,
      file.path(ts_dir, paste0("test-", safe_name, ".R")))
  }
  invisible(list(dir = fn_dir, is_fallback = is_fallback, macro_name = safe_name))
}

# Largest deterministic baseline (bytes) shown to the translator as reference.
TRANSLATOR_BASELINE_LIMIT <- 40000L

# The unit blocks of a staged module, which is all the translator is shown:
# the file scaffold (banner, bootstrap, module header comments) before the
# first unit marker goes, and a stub keeps its unit marker and its
# `# sas2r:untranslated` header but not the commented SAS source that
# stub_block() writes under it (the model has that source already).
translator_baseline_units <- function(lines) {
  scaffold <- c(BANNER, module_bootstrap())
  markers <- grep("^# --- sas2r:unit [0-9]+ ---$", lines)
  lines <- if (length(markers)) lines[markers[[1L]]:length(lines)]
           else lines[!lines %in% scaffold]
  drop <- logical(length(lines))
  for (i in grep("^# sas2r:untranslated (unit=[0-9]+ )?reason=", lines)) {
    if (!identical(lines[i + 1L], "# --- original SAS ---")) next
    # stub_block() prefixes every SAS line with "# " and closes the block
    # with a rule; the block ends before the next line it did not write.
    run <- i + 1L
    while (run < length(lines) && startsWith(lines[run + 1L], "# ") &&
           !grepl("^# --- sas2r:unit [0-9]+ ---$", lines[run + 1L])) run <- run + 1L
    closes <- which(lines[(i + 1L):run] == "# --------------------")
    if (length(closes)) drop[(i + 1L):(i + max(closes))] <- TRUE
  }
  lines[!drop]
}

# Packet lines presenting the rule-based output for a component as reference
# context, or NULL when there is nothing to show.
render_translator_baseline <- function(baseline_code, limit = TRANSLATOR_BASELINE_LIMIT) {
  if (is.null(baseline_code) || !length(baseline_code)) return(NULL)
  baseline_code <- translator_baseline_units(baseline_code)
  if (!length(baseline_code)) return(NULL)
  size <- nchar(paste(baseline_code, collapse = "\n"), type = "bytes")
  if (size > limit) {
    return(sprintf(
      "Deterministic baseline omitted: %d bytes exceeds the %d-byte reference limit.",
      size, as.integer(limit)
    ))
  }
  c(
    "Deterministic baseline (reference only): sas2r's rule-based output for this component.",
    "Units under `# sas2r:untranslated` have no baseline.",
    baseline_code
  )
}

#' Build translator context for a program component
#'
#' @param component_id Component identifier.
#' @param project `sas2r_project` object.
#' @param baseline `sas2r_transpilation` baseline.
#' @param graph Dependency graph.
#' @param schedule Dependency schedule.
#' @param outputs Inferred or explicit output contracts.
#' @param resolved_contracts Named list of upstream behavioral contracts.
#' @param config Project configuration list.
#' @param baseline_code Lines of the component's staged rule-based module, shown
#'   to the translator as reference; `NULL` leaves the baseline out.
#' @return Named list containing context for the translator agent.
#' @noRd
build_translator_context <- function(
  component_id,
  project,
  baseline = NULL,
  graph = NULL,
  schedule = NULL,
  outputs = NULL,
  resolved_contracts = list(),
  config = list(),
  macro_contract = component_macro_contract(project, graph, component_id),
  selected_revisions = list(),
  baseline_code = NULL
) {
  comp_nodes <- if (!is.null(graph$nodes)) graph$nodes[graph$nodes$component_id == component_id, , drop = FALSE] else NULL
  src_files <- if (!is.null(comp_nodes) && nrow(comp_nodes) > 0L) {
    unique(comp_nodes$source_file[!is.na(comp_nodes$source_file)])
  } else {
    character()
  }

  uids <- comp_nodes$original_index[!is.na(comp_nodes$original_index)]
  comp_stmts <- project$statements[project$statements$unit_id %in% uids, , drop = FALSE]

  sas_text <- if (nrow(comp_stmts) > 0L) {
    format_sas_statements(comp_stmts$text)
  } else if (length(src_files) > 0L && file.exists(src_files[1L])) {
    paste(read_sas_source(src_files[1L]), collapse = "\n")
  } else {
    ""
  }

  comments_text <- if (nrow(comp_stmts) > 0L) {
    uids <- unique(comp_stmts$unit_id[!is.na(comp_stmts$unit_id)])
    if (length(uids) > 0L) {
      paste(vapply(uids, function(uid) {
        render_unit_comment_evidence(project, uid)
      }, character(1)), collapse = "\n\n")
    } else {
      "(none attached)"
    }
  } else {
    "(none attached)"
  }

  upstream_deps <- if (!is.null(graph)) dependency_closure(graph, component_id) else character()
  upstream_contracts <- resolved_contracts[intersect(upstream_deps, names(resolved_contracts))]

  upstream_txt <- if (length(upstream_contracts) > 0L) {
    paste(vapply(names(upstream_contracts), function(cid) {
      uc <- upstream_contracts[[cid]]
      summary <- sprintf(
        "Component %s: parameters=(%s), reads=(%s), writes=(%s)",
        if (!is.null(uc$macro_contract)) paste0(cid, " (call ", uc$macro_contract$name, "; loaded by autoexec.R)") else cid,
        paste(vapply(uc$parameters %||% list(), function(p) p$name %||% "", character(1)), collapse = ", "),
        paste(uc$reads %||% character(), collapse = ", "),
        paste(uc$writes %||% character(), collapse = ", ")
      )
      if (!is.null(uc$macro_contract)) {
        paste(summary, render_macro_interface(uc$macro_contract), sep = "\n")
      } else summary
    }, character(1)), collapse = "\n")
  } else {
    "(none)"
  }

  call_sites <- list()
  if (!is.null(graph$edges) && nrow(graph$edges) > 0L && !is.null(comp_nodes) && nrow(comp_nodes) > 0L) {
    c_edges <- graph$edges[graph$edges$to %in% comp_nodes$node_id | graph$edges$from %in% comp_nodes$node_id, , drop = FALSE]
    call_edges <- c_edges[c_edges$type %in% c("calls_macro", "uses_function", "uses_format", "includes"), , drop = FALSE]
    if (nrow(call_edges) > 0L) {
      call_sites <- lapply(seq_len(nrow(call_edges)), function(idx) {
        list(
          type = call_edges$type[idx],
          detail = call_edges$detail[idx],
          resolution = call_edges$resolution[idx],
          line = call_edges$line[idx]
        )
      })
    }
  }

  call_txt <- if (length(call_sites) > 0L) {
    paste(vapply(call_sites, function(cs) sprintf("%s: %s (resolution: %s)", cs$type, cs$detail, cs$resolution), character(1)), collapse = "\n")
  } else {
    "(none)"
  }

  schemas <- infer_schemas(project)
  input_ds <- character()
  if (!is.null(project$lineage) && nrow(project$lineage) > 0L && nrow(comp_stmts) > 0L) {
    input_ds <- unique(project$lineage$dataset[project$lineage$unit_id %in% comp_stmts$unit_id & project$lineage$role == "reads"])
  }
  sch_txt <- if (length(input_ds) > 0L) {
    vapply(input_ds, function(ds) {
      s <- schemas[[ds]]
      if (is.null(s)) return(paste0(ds, ": unknown"))
      paste0(ds, ": ", paste(s$vars$var, s$vars$kind, sep = "=", collapse = ", "))
    }, character(1))
  } else {
    character()
  }

  lib_names <- if (!is.null(project$config$libraries)) names(project$config$libraries) else character()

  line_txt <- if (nrow(comp_stmts) > 0L && !is.null(comp_stmts$line_start) && length(stats::na.omit(comp_stmts$line_start))) {
    span <- range(stats::na.omit(comp_stmts$line_start))
    paste0("lines: ", span[1], "-", span[2])
  } else character()

  context_packet <- paste(
    c(
      "input schemas (inferred from code references):", sch_txt,
      paste("librefs:", paste(lib_names, collapse = ", ")),
      render_component_libraries(project, component_id),
      line_txt,
      "Source-owned macro interface:", render_macro_interface(macro_contract),
      "resolved upstream contracts:", upstream_txt,
      "Call translated upstream macro functions by their declared names and parameters. Their standalone R/macros files are loaded by autoexec.R; do not inline or redefine them in this component.",
      "known call sites:", call_txt,
      build_agent_guidance(project, component_id, list(macro_contract = macro_contract),
        selected_revisions, graph, config = config)$text,
      render_translator_baseline(baseline_code)
    ),
    collapse = "\n"
  )

  catalog <- agent_skill_catalog()
  u_types <- unique(comp_stmts$unit_type)
  is_macro <- "macro_def" %in% u_types
  procs <- unit_proc_names(comp_stmts)
  flags <- skill_flags_from_sas(sas_text)

  routing_ctx <- list(
    agent = "translator",
    unit_type = if (length(u_types)) u_types[1L] else "data_step",
    procs = procs,
    flags = flags,
    macros = if (is_macro) "macro_def" else character(0),
    semantic_rules = if (length(procs)) paste0("procs.", procs) else character(0),
    comparison_reasons = character(0),
    functions = character(0)
  )
  routed <- route_agent_skills(routing_ctx, catalog = catalog)
  rendered_skills <- render_agent_skills(routed)
  prompt_skill_hash <- worker_binding_hash("translator", routed, project_dir = project$project_dir)

  list(
    component_id = component_id,
    src_file = src_files[1L] %||% NA_character_,
    comp_stmts = comp_stmts,
    sas_text = sas_text,
    comments_text = comments_text,
    resolved_dependencies = upstream_deps,
    upstream_contracts = upstream_contracts,
    call_sites = call_sites,
    schemas = schemas,
    routed_skills = routed,
    rendered_skills = rendered_skills,
    prompt_skill_hash = prompt_skill_hash,
    context_packet = context_packet
  )
}

#' Aggregate deterministic and model facts into one behavioral contract
#'
#' @param component_id Component identifier.
#' @param project `sas2r_project` object.
#' @param baseline `sas2r_transpilation` baseline.
#' @param graph Dependency graph.
#' @param schedule Dependency schedule.
#' @param outputs Inferred/declared output contracts.
#' @param r_code_text Generated R source code string.
#' @param tr_data Optional structured translator output conforming to `program_translation_v1`.
#' @param resolved_contracts List of upstream behavioral contracts.
#' @param helper_hash Hash of runtime helpers.
#' @param prompt_skill_hash Hash of prompt and skills used.
#' @param macro_contract Parsed source macro contract, when available.
#' @param allowlist Configured package namespaces, or NULL for the default.
#' @return A behavioral contract list.
#' @noRd
build_behavioral_contract <- function(
  component_id,
  project,
  baseline = NULL,
  graph = NULL,
  schedule = NULL,
  outputs = NULL,
  r_code_text = "",
  tr_data = NULL,
  resolved_contracts = list(),
  helper_hash = NULL,
  prompt_skill_hash = NULL,
  macro_contract = component_macro_contract(project, graph, component_id),
  allowlist = project$config$allowlist
) {
  comp_nodes <- if (!is.null(graph$nodes)) graph$nodes[graph$nodes$component_id == component_id, , drop = FALSE] else NULL
  uids <- if (!is.null(comp_nodes) && nrow(comp_nodes) > 0L) comp_nodes$original_index[!is.na(comp_nodes$original_index)] else integer()

  det_reads <- character()
  det_writes <- character()
  if (!is.null(project$lineage) && nrow(project$lineage) > 0L && length(uids) > 0L) {
    det_reads <- unique(project$lineage$dataset[project$lineage$unit_id %in% uids & project$lineage$role == "reads"])
    det_writes <- unique(project$lineage$dataset[project$lineage$unit_id %in% uids & project$lineage$role == "creates"])
  }

  reads <- unique(c(det_reads, unlist(tr_data$reads %||% character())))
  writes <- unique(c(det_writes, unlist(tr_data$writes %||% character())))
  reads <- reads[!is.na(reads) & nzchar(reads)]
  writes <- writes[!is.na(writes) & nzchar(writes)]

  params <- list()
  defaults <- structure(list(), names = character(0))
  macro_contract_obj <- macro_contract
  if (!is.null(macro_contract_obj) && nrow(macro_contract_obj$parameters) > 0L) {
    mp <- macro_contract_obj$parameters
    params <- lapply(seq_len(nrow(mp)), function(i) {
      list(
        name = mp$name[i],
        type = if (identical(mp$default_status[i], "known") && is.numeric(mp$r_default[[i]])) "numeric" else "character",
        required = identical(mp$default_status[i], "unresolved") && !nzchar(mp$sas_default[i]),
        default = if (mp$default_status[i] == "known") mp$r_default[[i]] else NULL
      )
    })
    known_i <- which(mp$default_status == "known")
    if (length(known_i) > 0L) {
      defaults <- stats::setNames(lapply(known_i, function(i) mp$r_default[[i]]), mp$name[known_i])
    }
  }

  if (is.null(macro_contract_obj) && length(params) == 0L && !is.null(tr_data$parameters) && length(tr_data$parameters) > 0L) {
    params <- as.list(tr_data$parameters)
    if (!is.null(tr_data$defaults)) defaults <- tr_data$defaults
  }
  if (!is.null(macro_contract_obj) && any(comp_nodes$type == "macro")) {
    macro_contract_obj$standalone <- TRUE
  }

  known_call_sites <- list()
  if (!is.null(graph$edges) && nrow(graph$edges) > 0L && !is.null(comp_nodes) && nrow(comp_nodes) > 0L) {
    c_edges <- graph$edges[graph$edges$from %in% comp_nodes$node_id | graph$edges$to %in% comp_nodes$node_id, , drop = FALSE]
    if (nrow(c_edges) > 0L) {
      call_types <- c("calls_macro", "uses_function", "uses_format", "includes")
      call_edges <- c_edges[c_edges$type %in% call_types, , drop = FALSE]
      if (nrow(call_edges) > 0L) {
        known_call_sites <- lapply(seq_len(nrow(call_edges)), function(idx) {
          list(
            edge_id = call_edges$edge_id[idx],
            type = call_edges$type[idx],
            from = call_edges$from[idx],
            to = call_edges$to[idx],
            resolution = call_edges$resolution[idx],
            source_file = call_edges$source_file[idx],
            line = call_edges$line[idx],
            detail = call_edges$detail[idx]
          )
        })
      }
    }
  }

  dependency_functions <- character()
  if (length(known_call_sites)) {
    incoming <- Filter(function(site) site$type %in% c("calls_macro", "uses_function") &&
      identical(site$resolution, "resolved") && site$to %in% comp_nodes$node_id,
      known_call_sites)
    dependency_functions <- unique(vapply(incoming, function(site) site$detail, character(1)))
  }
  helper_use <- reconcile_helper_use(r_code_text, tr_data$helper_use,
                                    dependency_functions, allowlist = allowlist)

  resolved_deps <- if (!is.null(graph)) dependency_closure(graph, component_id) else character()
  suspected_deps <- unique(unlist(tr_data$suspected_dependencies %||% character()))
  if (!is.null(graph$edges) && nrow(graph$edges) > 0L && !is.null(comp_nodes) && nrow(comp_nodes) > 0L) {
    susp_edges <- graph$edges[graph$edges$to %in% comp_nodes$node_id & graph$edges$resolution %in% c("suspected", "ambiguous", "dynamic", "unresolved"), , drop = FALSE]
    if (nrow(susp_edges) > 0L) {
      suspected_deps <- unique(c(suspected_deps, susp_edges$detail[nzchar(susp_edges$detail)]))
    }
  }
  suspected_deps <- suspected_deps[!is.na(suspected_deps) & nzchar(suspected_deps)]

  affected_outputs <- unique(unlist(tr_data$affected_outputs %||% character()))
  if (!is.null(outputs) && nrow(outputs) > 0L) {
    out_matches <- outputs$target_key[outputs$producer_node_id %in% comp_nodes$node_id |
                                      tolower(outputs$logical_name) %in% tolower(writes) |
                                      tolower(outputs$target_key) %in% tolower(writes)]
    affected_outputs <- unique(c(affected_outputs, out_matches))
  }
  affected_outputs <- affected_outputs[!is.na(affected_outputs) & nzchar(affected_outputs)]

  side_effects <- unique(unlist(tr_data$side_effects %||% character()))
  if (any(grepl("lib_write\\(", r_code_text))) {
    side_effects <- unique(c(side_effects, "dataset_write"))
  }
  if (any(grepl("apply_format|format", r_code_text))) {
    side_effects <- unique(c(side_effects, "format_use"))
  }
  side_effects <- side_effects[!is.na(side_effects) & nzchar(side_effects)]

  uncertainty <- tr_data$uncertainty %||% list()

  sas_text <- component_source_text(graph, component_id)

  source_h <- migration_hash(sas_text)
  r_h <- migration_hash(r_code_text)

  upstream_r_hashes <- stats::setNames(
    vapply(resolved_deps, function(d) {
      if (!is.null(resolved_contracts[[d]]) && !is.null(resolved_contracts[[d]]$binding$r_hash)) {
        resolved_contracts[[d]]$binding$r_hash
      } else ""
    }, character(1)),
    resolved_deps
  )
  all_r_hashes <- c(stats::setNames(r_h, component_id), upstream_r_hashes)

  closure_h <- if (!is.null(graph)) {
    dependency_closure_hashes(
      graph,
      selected_revision_hashes = all_r_hashes,
      helper_hash = helper_hash,
      prompt_skill_hash = prompt_skill_hash
    )[[component_id]]
  } else {
    migration_hash(list(component_id = component_id, r_hash = r_h))
  }

  binding <- new_component_binding(
    source_hash = source_h,
    r_hash = r_h,
    helper_hash = if (!is.null(helper_hash) && nzchar(helper_hash)) helper_hash else migration_hash(""),
    prompt_skill_hash = if (!is.null(prompt_skill_hash) && nzchar(prompt_skill_hash)) prompt_skill_hash else migration_hash(""),
    dependency_closure_hash = closure_h
  )

  result <- new_behavioral_contract(
    component_id = component_id,
    sas_text = sas_text,
    parameters = params,
    defaults = defaults,
    reads = reads,
    writes = writes,
    side_effects = side_effects,
    helper_use = helper_use,
    dependency_functions = dependency_functions,
    known_call_sites = known_call_sites,
    resolved_dependencies = resolved_deps,
    suspected_dependencies = suspected_deps,
    affected_outputs = affected_outputs,
    uncertainty = uncertainty,
    binding = binding,
    macro_contract = macro_contract_obj,
    flags = if (any(comp_nodes$type == "macro")) {
      if (is.null(tr_data)) "macro_deferred" else c("llm_authored", "macro_semantics_unverified")
    } else character()
  )
  discoveries <- unique(as.character(unlist(tr_data$discovered_dependencies %||% character())))
  if (length(discoveries)) result$discovered_dependencies <- discoveries
  result$helper_use_declared <- unlist(tr_data$helper_use %||% character())
  result
}

#' Generate a single program revision and behavioral contract
#'
#' @param component_id Component identifier.
#' @param project `sas2r_project` object.
#' @param baseline `sas2r_transpilation` baseline.
#' @param graph Dependency graph.
#' @param schedule Dependency schedule.
#' @param outputs Inferred/declared output contracts.
#' @param llm Optional `sas2r_llm` instance.
#' @param paths Migration paths list.
#' @param resolved_contracts List of upstream behavioral contracts.
#' @param revision_id Revision identifier (default "r1").
#' @param config Project configuration list.
#' @param usage_budget Optional usage budget.
#' @param ... Additional arguments.
#' @return A list with `component_id`, `revision_id`, `r_path`, `contract_path`, `contract`, `status`, `checks`.
#' @noRd
generate_program_revision <- function(
  component_id,
  project,
  baseline,
  graph,
  schedule,
  outputs,
  llm = NULL,
  paths,
  resolved_contracts = list(),
  revision_id = "r1",
  config = list(),
  usage_budget = NULL,
  selected_revisions = list(),
  helper_code = NULL,
  ...
) {
  comp_nodes <- if (!is.null(graph$nodes)) graph$nodes[graph$nodes$component_id == component_id, , drop = FALSE] else NULL
  src_files <- if (!is.null(comp_nodes) && nrow(comp_nodes) > 0L) {
    unique(comp_nodes$source_file[!is.na(comp_nodes$source_file)])
  } else {
    character()
  }

  uids <- comp_nodes$original_index[!is.na(comp_nodes$original_index)]
  staged_rel <- NULL
  if (!is.null(baseline$manifest) && nrow(baseline$manifest) > 0L) {
    m_rows <- baseline$manifest[
      if (length(uids)) baseline$manifest$unit_id %in% uids else
        baseline$manifest$file %in% src_files, ,
      drop = FALSE
    ]
    if (nrow(m_rows) > 0L) {
      staged_rel <- m_rows$staged_file[!is.na(m_rows$staged_file)][1L]
    }
  }
  if (is.null(staged_rel) || is.na(staged_rel) || !nzchar(staged_rel)) {
    staged_rel <- paste0(component_id, ".R")
  }

  staged_path <- file.path(baseline$out_dir %||% paths$staging %||% paths$root, staged_rel)
  staged_lines <- if (file.exists(staged_path)) readLines(staged_path, warn = FALSE) else character()

  comp_units <- if (!is.null(baseline$manifest) && nrow(baseline$manifest) > 0L) {
    baseline$manifest[baseline$manifest$staged_file == staged_rel, , drop = FALSE]
  } else NULL

  stubs <- if (!is.null(comp_units) && nrow(comp_units) > 0L) {
    comp_units[comp_units$tier == "stub" & !(comp_units$reason %in% "include_site_not_emitted"), , drop = FALSE]
  } else NULL

  needs_agent <- !is.null(stubs) && nrow(stubs) > 0L && !is.null(llm)

  tr_data <- NULL
  prompt_skill_h <- NULL
  # NA when the deterministic core translated alone; otherwise the terminal
  # run_agent() status, so a run whose LLM calls all failed cannot read like a
  # successful deterministic run.
  agent_status <- NA_character_
  retry_record <- NULL
  macro_contract <- component_macro_contract(project, graph, component_id)

  if (needs_agent) {
    specs <- load_agent_specs(project_dir = project$project_dir)
    spec <- as.list(specs$translator)

    ctx <- build_translator_context(
      component_id = component_id,
      project = project,
      baseline = baseline,
      graph = graph,
      schedule = schedule,
      outputs = outputs,
      resolved_contracts = resolved_contracts,
      config = config,
      macro_contract = macro_contract,
      selected_revisions = selected_revisions,
      baseline_code = if (isTRUE(getOption("sas2r.translator_baseline_reference", TRUE)) &&
        !is.null(comp_units) && any(comp_units$tier == "t1")) staged_lines
    )
    prompt_skill_h <- ctx$prompt_skill_hash

    # A source program can contain definitions and invoke them. Only standalone
    # macro components receive the definitions-only translation instruction.
    is_macro <- isTRUE(macro_contract$standalone)
    if (is_macro) {
      spec$prompt <- spec$prompt_macro %||% "translator-macro.md"
    }

    audit_context <- list(
      role = "translator",
      component_id = component_id,
      revision_id = revision_id,
      round = 0L,
      purpose = "program_translation",
      resolved_dependencies = ctx$resolved_dependencies,
      upstream_contracts = ctx$upstream_contracts
    )

    prompt_vars <- list(
      style = render_style_guidance(config),
      allowlist = paste(normalize_package_allowlist(config$allowlist), collapse = ", "),
      unit = ctx$sas_text,
      context = ctx$context_packet,
      comments = ctx$comments_text,
      skills = ctx$rendered_skills
    )

    # Tools get the same context the unit-level path builds. Without the
    # component's statements, schemas, and macro index, find_macro answered
    # not_in_index for macros that were sitting in the configured search path
    # while still consuming the tool-call budget.
    uids <- comp_nodes$original_index[!is.na(comp_nodes$original_index)]
    unit_stmts <- if (!is.null(project$statements) && length(uids)) {
      project$statements[project$statements$unit_id %in% uids, , drop = FALSE]
    } else {
      NULL
    }
    macro_idx <- project_macro_index(project, config)
    tool_ctx <- list(
      agent_role = "translator",
      component_id = component_id, graph = graph, selected_revisions = selected_revisions,
      project = project,
      unit_stmts = unit_stmts,
      schemas = tryCatch(infer_schemas(project), error = function(e) list()),
      config = config,
      macro_index = macro_idx
    )

    agent_res <- run_agent(
      spec = spec,
      llm = llm,
      tools = build_tools(spec, tool_ctx),
      user_content = "Translate this SAS component and emit its behavioral contract.",
      log_dir = paths$logs %||% file.path(paths$root, ".sas2r"),
      prompt_vars = prompt_vars,
      audit_context = audit_context,
      usage_budget = usage_budget
    )
    agent_status <- agent_res$status %||% "unknown"

    if (identical(agent_res$status, "ok") && !is.null(agent_res$data)) {
      tr_data <- agent_res$data

      parsed_chk <- tryCatch(parse(text = tr_data$r_code), error = function(e) e)
      lint_chk <- if (!inherits(parsed_chk, "error")) lint_r_code(tr_data$r_code, allowlist = config$allowlist) else NULL
      has_lint_err <- !is.null(lint_chk) && any(lint_chk$level == "error")
      binding_errors <- check_component_library_assignments(tr_data$r_code, project, component_id)

      if (inherits(parsed_chk, "error") || has_lint_err || length(binding_errors)) {
        err_msg <- if (inherits(parsed_chk, "error")) conditionMessage(parsed_chk) else paste(
          sprintf("%s: %s", lint_chk$kind[lint_chk$level == "error"],
                  lint_chk$detail[lint_chk$level == "error"]), collapse = "; ")
        err_msg <- paste(c(err_msg[nzchar(err_msg)], binding_errors), collapse = "; ")
        retry_record <- list(errors = err_msg, dynamic_code = grepl("banned_function.*(parse|eval)", err_msg),
          prior_revision_id = revision_id)
        retry_res <- run_agent(
          spec = spec,
          llm = llm,
          tools = build_tools(spec, tool_ctx),
          user_content = paste(
            "Previous code failed mechanical checks:", err_msg,
            "Correct the code below and return the complete translation JSON.",
            "Do not call library() or require(); qualify package functions (for example dplyr::mutate) and use the base |> pipe.",
            "Use supported operations; do not replace banned parse/eval with a handwritten general interpreter. Keep unsupported behavior explicit without dropping required source logic.",
            "Previous R code:", tr_data$r_code, sep = "\n"),
          log_dir = paths$logs %||% file.path(paths$root, ".sas2r"),
          prompt_vars = prompt_vars,
          audit_context = utils::modifyList(audit_context, list(purpose = "mechanical_retry")),
          usage_budget = usage_budget
        )
        if (identical(retry_res$status, "ok") && !is.null(retry_res$data)) {
          tr_data <- retry_res$data
        }
      }

      is_entry <- any(comp_nodes$type == "source_unit") && !any(comp_nodes$type == "setup")
      staged_lines <- if (is_entry && !any(grepl("sas2r bootstrap", tr_data$r_code))) {
        c(BANNER, "", module_bootstrap(), "", tr_data$r_code)
      } else {
        c(BANNER, "", tr_data$r_code)
      }
    }
  }

  if (is.null(prompt_skill_h)) {
    prompt_skill_h <- worker_binding_hash("translator", list(), project_dir = project$project_dir)
  }

  final_r_code_lines <- if (length(staged_lines) > 0L) staged_lines else c(BANNER, "")
  final_r_code_text <- paste(final_r_code_lines, collapse = "\n")

  helper_path <- file.path(baseline$out_dir %||% paths$staging %||% paths$root, "sas2r-helpers.R")
  helper_h <- if (!is.null(helper_code)) migration_hash(helper_code) else if (file.exists(helper_path)) {
    migration_hash(paste(readLines(helper_path, warn = FALSE), collapse = "\n"))
  } else {
    baseline$runtime_hash %||% migration_hash("")
  }

  contract <- build_behavioral_contract(
    component_id = component_id,
    project = project,
    baseline = baseline,
    graph = graph,
    schedule = schedule,
    outputs = outputs,
    r_code_text = final_r_code_text,
    tr_data = tr_data,
    resolved_contracts = resolved_contracts,
    helper_hash = helper_h,
    prompt_skill_hash = prompt_skill_h,
    macro_contract = macro_contract,
    allowlist = config$allowlist
  )

  rev_dir <- file.path(paths$component_revisions, component_id, "revisions", revision_id)
  dir.create(rev_dir, recursive = TRUE, showWarnings = FALSE)

  r_path <- file.path(rev_dir, "program.R")
  contract_path <- file.path(rev_dir, "contract.json")

  atomic_write_file(function(path) writeLines(final_r_code_lines, path), r_path)
  atomic_write_json(contract, contract_path)

  registry_path <- file.path(baseline$out_dir %||% paths$staging %||% paths$root, "autoexec.R")
  checks <- check_program_revision(r_path, contract = contract, registry = registry_path,
    allowlist = config$allowlist, project = project)

  list(
    component_id = component_id,
    revision_id = revision_id,
    staged_file = staged_rel,
    r_path = r_path,
    contract_path = contract_path,
    contract = contract,
    binding = contract$binding,
    status = if (isTRUE(checks$pass)) "ok" else "check_failed",
    checks = checks,
    r_code = final_r_code_text,
    agent_status = agent_status,
    mechanical_retry = retry_record
  )
}

#' Generate all program revisions in stable dependency order
#'
#' @param project A `sas2r_project` object.
#' @param baseline Optional `sas2r_transpilation` baseline from `sas_transpile()`.
#' @param graph Optional dependency graph from `build_dependency_graph()`.
#' @param schedule Optional stable schedule from `stable_dependency_schedule()`.
#' @param outputs Optional output contracts from `infer_output_contracts()`.
#' @param llm Optional `sas2r_llm` instance for agent-assisted translation.
#' @param paths Optional named list of migration paths from `migration_paths()`.
#' @param config Configuration list.
#' @param usage_budget Optional shared usage budget.
#' @param ... Additional arguments.
#' @return A tibble with columns `component_id`, `revision_id`, `r_path`, `contract_path`, `contract`, `status`, `checks`.
#' @noRd
generate_program_revisions <- function(
  project,
  baseline = NULL,
  graph = NULL,
  schedule = NULL,
  outputs = NULL,
  llm = NULL,
  paths = NULL,
  config = list(),
  usage_budget = NULL,
  ...
) {
  if (is.null(paths)) {
    paths <- init_migration_paths(tempfile("sas2r-revisions-"))
  } else if (is.character(paths)) {
    paths <- init_migration_paths(paths)
  } else if (is.list(paths)) {
    init_migration_paths(paths$root)
  }

  if (is.null(baseline)) {
    baseline <- sas_transpile(project, paths$staging %||% paths$root)
  }
  if (is.null(outputs)) {
    outputs <- infer_output_contracts(project, overrides = config$outputs)
  }
  if (is.null(graph)) {
    graph <- build_dependency_graph(project, output_contracts = outputs)
  }
  if (is.null(schedule)) {
    schedule <- stable_dependency_schedule(graph)
  }

  resolved_contracts <- list()
  rows <- list()

  if (nrow(schedule) > 0L) {
    for (cid in schedule$component_id) {
      rev_res <- generate_program_revision(
        component_id = cid,
        project = project,
        baseline = baseline,
        graph = graph,
        schedule = schedule,
        outputs = outputs,
        llm = llm,
        paths = paths,
        resolved_contracts = resolved_contracts,
        revision_id = "r1",
        config = config,
        usage_budget = usage_budget,
        ...
      )
      resolved_contracts[[cid]] <- rev_res$contract
      rows[[length(rows) + 1L]] <- rev_res
    }
  }

  if (length(rows) == 0L) {
    return(tibble::tibble(
      component_id = character(),
      revision_id = character(),
      r_path = character(),
      contract_path = character(),
      contract = list(),
      status = character(),
      checks = list(),
      agent_status = character()
    ))
  }

  tibble::tibble(
    component_id = vapply(rows, `[[`, character(1), "component_id"),
    revision_id = vapply(rows, `[[`, character(1), "revision_id"),
    r_path = vapply(rows, `[[`, character(1), "r_path"),
    contract_path = vapply(rows, `[[`, character(1), "contract_path"),
    contract = lapply(rows, `[[`, "contract"),
    status = vapply(rows, function(r) r$status %||% "ok", character(1)),
    checks = lapply(rows, `[[`, "checks"),
    agent_status = vapply(rows, function(r) r$agent_status %||% NA_character_, character(1))
  )
}
