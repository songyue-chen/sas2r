# Made-up migrations for the AI bug triage tests. Every dataset here is
# invented in the test; nothing is read from a real run.

# One or more SAS programs, the R selected for each, a generated output and a
# reference per program, assessed by the real gate. Program `i` reads
# `raw.input` and writes `work.out<i>`. `origin` is the status the translator
# recorded: "ok" for a model translation, NA when the rules translated alone.
triage_scenario <- function(sas, r, generated, reference,
                            input = data.frame(id = 1:3, value = c(10, 11, 12)),
                            origin = "ok", rules = list(), envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)
  n <- length(sas)
  several <- function(x) if (is.data.frame(x)) rep(list(x), n) else x
  generated <- several(generated)
  reference <- several(reference)
  inputs <- file.path(root, "inputs")
  dir.create(inputs)
  saveRDS(input, file.path(inputs, "input.rds"))
  ids <- sprintf("p%02d", seq_len(n))
  for (i in seq_len(n)) writeLines(sas[[i]], file.path(root, paste0(ids[[i]], ".sas")))
  outputs <- paste0("work.out", seq_len(n))
  references <- stats::setNames(as.list(file.path(root, paste0("reference", seq_len(n), ".rds"))), outputs)
  for (i in seq_len(n)) saveRDS(reference[[i]], references[[i]])
  # The outputs and their references are configured on the project, as a run
  # configures them, so the gate records its own decisions for each output.
  config <- list(libraries = list(raw = list(path = inputs, engine = "rds")),
    outputs = list(datasets = outputs, references = references))
  project <- sas_project(root, config = config)
  state <- new_migration_state(project, file.path(root, "migration"), config = config)
  attempt_dir <- file.path(root, "attempt")
  dir.create(file.path(attempt_dir, "work"), recursive = TRUE)
  for (i in seq_len(n)) {
    state$selected_revisions[[ids[[i]]]] <- list(component_id = ids[[i]], revision_id = "r1",
      r_code = r[[i]], staged_file = paste0(ids[[i]], ".R"), agent_status = rep_len(origin, n)[[i]])
    saveRDS(generated[[i]], file.path(attempt_dir, "work", paste0("out", i, ".rds")))
  }
  state$output_contracts <- project$output_contracts
  state$comparison_rules <- c(list(references = references), rules)
  state$assessment <- assess_final_outputs(state$output_contracts,
    list(attempt_dir = attempt_dir, completed = TRUE, passed = TRUE), state$graph,
    project = state$project, comparison_rules = state$comparison_rules)
  state$config$migration$bug_reports <- TRUE
  state
}

# The facts a triage request carries, parsed back from its text.
triage_request_facts <- function(request) {
  lines <- strsplit(request$messages[[2L]]$content, "\n", fixed = TRUE)[[1L]]
  stopifnot(identical(lines[[1L]], "Facts:"))
  jsonlite::fromJSON(lines[[2L]], simplifyVector = FALSE)
}

triage_column <- function(facts, name, evidence = 1L) {
  Filter(function(column) identical(column$name, name), facts$evidence[[evidence]]$columns)[[1L]]
}

# A stand-in model that follows the signs listed in the triage prompt and
# nothing else. It reads only the request, so a case it gets right is a case
# the request alone separates. A column whose facts carry no sign that points
# to a package step yields no finding.
triage_sign_reader <- function(request) {
  text <- request$messages[[2L]]$content
  facts <- triage_request_facts(request)
  origins <- vapply(facts$programs, function(program) program$origin, "")
  rule_written <- length(origins) > 0L && all(origins %in% c("rule_based", "rule_based_model_unavailable"))
  findings <- list()
  for (id in names(facts$evidence)) {
    for (column in facts$evidence[[id]]$columns) {
      told <- unique(vapply(column$translator_told, function(x) x$type, ""))
      documented <- vapply(unlist(column$helpers), function(helper)
        grepl(paste0("\n", helper, ": "), text, fixed = TRUE), logical(1))
      step <- if (identical(column$representation_only, "yes")) {
        "write_and_read_conversions"
      } else if (length(told) == 1L && !identical(told, column$reference_type) &&
          identical(told, column$generated_type)) {
        "facts_given_to_translator"
      } else if (identical(column$generated_code, "passed_through") &&
          identical(column$extent, "missing_only")) {
        "reading_inputs_and_references"
      } else if (identical(column$generated_code, "assigned") && length(documented) && all(documented) &&
          identical(facts$runtime_helpers, "package")) {
        "runtime_helpers"
      } else if (identical(column$difference, "missing_in_generated") &&
          identical(column$sas_source, "assigned") && rule_written) {
        "rule_based_translation_and_support_files"
      }
      if (is.null(step) || !is.null(findings[[id]])) next
      findings[[id]] <- list(evidence_id = id, responsible_step = step,
        explanation = "The facts for this output point to a step sas2r owns.",
        expected = "The step should leave the values as the SAS source defines them.",
        uncertainty = "The inputs and the reference were not inspected.",
        next_step = "Check the step with made-up values.", repro_r = "")
    }
  }
  list(findings = unname(findings))
}

# Run triage on a scenario with the sign-following stand-in as the model.
triage_with_sign_reader <- function(state) {
  request <- NULL
  llm <- new_llm(function(req, audit_context = list()) {
    request <<- req
    new_llm_response(status = "completed", action = "final", data = triage_sign_reader(req),
      request = req, provider = "mock", resolved_model = "diagnosis-test")
  }, provider = "mock", model = "diagnosis-test")
  diagnosis <- suppressMessages(migration_package_bug_diagnosis(state, llm))
  list(diagnosis = diagnosis, request = request, facts = triage_request_facts(request))
}
