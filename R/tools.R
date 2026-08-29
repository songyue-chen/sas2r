#' Create a budget-enforcing tool wrapper
#'
#' @param name Tool name.
#' @param fn Execution function taking `args`.
#' @param max_calls Budget cap.
#' @param schema Closed JSON Schema for tool arguments.
#' @param description Provider-visible tool description.
#' @return List with `name`, closed `schema`, and `call(args)`.
#' @noRd
make_tool <- function(name, fn, max_calls, schema = NULL, description = NULL) {
  force(name)
  force(fn)
  force(max_calls)
  schema <- schema %||% tool_argument_schema(name)
  if (is.null(schema)) {
    schema <- list(type = "object", properties = list(),
                   additionalProperties = FALSE)
  }
  if (!identical(schema$type, "object") ||
      !identical(schema$additionalProperties, FALSE)) {
    cli::cli_abort("tool {.val {name}} requires a closed object argument schema",
                   class = "sas2r_tool_schema_error")
  }
  calls <- 0L; refused <- FALSE
  list(name = name, schema = schema, description = description %||% name,
       call = function(args) {
    validate_tool_arguments(args, schema, name)
    calls <<- calls + 1L
    if (calls > max_calls) {
      if (!refused) { refused <<- TRUE; return(list(error = "budget_exhausted")) }
      cli::cli_abort("tool {.val {name}} exceeded its budget after refusal",
                     class = "sas2r_tool_budget_error")
    }
    fn(args)
  })
}

tool_string <- function() list(type = "string", minLength = 1L)
tool_string_array <- function() list(type = "array", items = tool_string())
closed_tool_schema <- function(properties = list(), required = character()) {
  schema <- list(
    type = "object", properties = properties,
    additionalProperties = FALSE
  )
  if (length(required)) schema$required <- required
  schema
}

TOOL_ARGUMENT_SCHEMAS <- list(
  read_unit_context = closed_tool_schema(),
  query_project_graph = closed_tool_schema(
    list(dataset = tool_string()), "dataset"
  ),
  lookup_rulebook = closed_tool_schema(list(
    name = tool_string(), functions = tool_string_array(),
    operators = tool_string_array(), procs = tool_string_array()
  ), "name"),
  find_macro = closed_tool_schema(list(name = tool_string()), "name"),
  get_macro_source = closed_tool_schema(list(name = tool_string()), "name"),
  list_macro_files = closed_tool_schema(),
  search_docs = closed_tool_schema(list(
    construct = tool_string(), package = tool_string(), topic = tool_string()
  )),
  search_skills = closed_tool_schema(list(query = tool_string()), "query"),
  read_skill = closed_tool_schema(list(name = tool_string()), "name"),
  read_comparison_report = closed_tool_schema(list(report_id = tool_string()), "report_id")
)

TOOL_DESCRIPTIONS <- c(
  read_unit_context = "Read the current SAS unit and inferred input schemas.",
  query_project_graph = "Query code-derived dataset lineage.",
  lookup_rulebook = "Look up deterministic SAS-to-R semantic rules.",
  find_macro = "Find indexed SAS macro definitions by name.",
  get_macro_source = "Read bounded source for an indexed SAS macro.",
  list_macro_files = "List indexed SAS macro source filenames.",
  search_docs = "Search the configured local documentation mirror.",
  search_skills = "Search registered skills in the curated catalogue.",
  read_skill = "Read full guidance and provenance for a registered skill.",
  read_comparison_report = "Read a bounded output comparison report by registered report ID."
)

tool_argument_schema <- function(name) TOOL_ARGUMENT_SCHEMAS[[name]]

validate_tool_arguments <- function(args, schema, name) {
  if (!is.list(args)) {
    cli::cli_abort("tool {.val {name}} arguments must be an object",
                   class = "sas2r_tool_arguments_error")
  }
  required <- as.character(unlist(schema$required %||% character()))
  missing <- setdiff(required, names(args))
  extra <- setdiff(names(args), names(schema$properties %||% list()))
  if (length(missing) || length(extra)) {
    details <- c(
      if (length(missing)) paste0("missing: ", paste(missing, collapse = ", ")),
      if (length(extra)) paste0("unexpected: ", paste(extra, collapse = ", "))
    )
    cli::cli_abort(
      "invalid arguments for tool {.val {name}} ({paste(details, collapse = '; ')})",
      class = "sas2r_tool_arguments_error"
    )
  }
  errors <- character()
  properties <- schema$properties %||% list()
  for (argument in intersect(names(args), names(properties))) {
    errors <- validate_schema_value(
      args[[argument]], properties[[argument]],
      paste0("$.", argument), errors
    )
  }
  if (length(errors)) {
    cli::cli_abort(
      "invalid arguments for tool {.val {name}} ({paste(errors, collapse = '; ')})",
      class = "sas2r_tool_arguments_error"
    )
  }
  invisible(args)
}

TOOL_IMPLS <- list(
  read_unit_context = function(ctx) function(args) {
    inputs <- if (!is.null(ctx$project$lineage) && !is.null(ctx$unit_stmts)) {
      unique(ctx$project$lineage$dataset[
        ctx$project$lineage$unit_id %in% ctx$unit_stmts$unit_id &
        ctx$project$lineage$role == "reads"])
    } else character()
    schemas <- lapply(inputs, function(ds) {
      s <- ctx$schemas[[ds]]
      if (is.null(s)) NULL else as.list(s$vars)
    })
    names(schemas) <- inputs
    sas_txt <- if (!is.null(ctx$unit_stmts)) format_sas_statements(ctx$unit_stmts$text) else ""
    line_range <- if (!is.null(ctx$unit_stmts) && !is.null(ctx$unit_stmts$line_start)) {
      range(ctx$unit_stmts$line_start)
    } else c(1L, 1L)
    list(sas = sas_txt,
         lines = line_range,
         input_schemas = schemas, data = NULL)
  },
  query_project_graph = function(ctx) function(args) {
    ds <- tolower(args$dataset %||% "")
    if (!is.null(ctx$project$lineage)) {
      as.list(ctx$project$lineage[ctx$project$lineage$dataset == ds, ])
    } else list()
  },
  lookup_rulebook = function(ctx) function(args) {
    rb <- load_rulebook()
    registry <- rb$semantics
    make_entry <- function(kind, name, target) {
      semantic <- registry$rules[[paste0(kind, ".", name)]]
      if (is.null(semantic)) {
        return(list(target = unname(target), semantic = NULL))
      }
      list(
        target = unname(target),
        semantic_delta = list(
          sas_default = semantic$sas_default,
          r_default = semantic$r_default
        ),
        classification = semantic$classification,
        strategy = semantic$strategy,
        implementation_status = semantic$implementation_status,
        risk = semantic$risk,
        scope = semantic$scope,
        source_urls = vapply(semantic$sources, `[[`, "", "url"),
        semantic = semantic
      )
    }
    requested_functions <- args$functions %||% args$name %||% character()
    fns <- intersect(unique(tolower(requested_functions)), names(rb$functions))
    requested_operators <- args$operators
    ops <- if (is.null(requested_operators)) {
      names(rb$operators)
    } else {
      intersect(unique(tolower(requested_operators)), names(rb$operators))
    }
    requested_procs <- args$procs %||% character()
    procs <- intersect(unique(tolower(requested_procs)), names(rb$procs))
    function_semantics <- stats::setNames(lapply(
      fns, function(name) make_entry("functions", name, rb$functions[[name]])
    ), fns)
    operator_semantics <- stats::setNames(lapply(
      ops, function(name) make_entry("operators", name, rb$operators[[name]])
    ), ops)
    proc_semantics <- stats::setNames(lapply(
      procs, function(name) {
        make_entry("procs", name, rb$procs[[name]]$emitter %||% NA_character_)
      }
    ), procs)
    list(
      functions = as.list(rb$functions[fns]),
      operators = as.list(rb$operators[ops]),
      procs = stats::setNames(lapply(
        procs, function(name) {
          list(
            target = rb$procs[[name]]$emitter %||% NA_character_,
            status = rb$procs[[name]]$status,
            semantic = registry$rules[[paste0("procs.", name)]]
          )
        }
      ), procs),
      semantics = list(
        functions = function_semantics,
        operators = operator_semantics,
        procs = proc_semantics
      )
    )
  },
  find_macro = function(ctx) function(args) {
    nm <- tolower(args$name %||% "")
    if (is.null(ctx$macro_index) || !nrow(ctx$macro_index)) return(list(error = "not_in_index"))
    hits <- ctx$macro_index[ctx$macro_index$name == nm, ]
    if (!nrow(hits)) return(list(error = "not_in_index"))
    list(hits = lapply(seq_len(nrow(hits)), function(k) list(
      file = hits$file[k], line_start = hits$line_start[k],
      line_end = hits$line_end[k], verified = TRUE,
      shadowed_by = if (k > 1L) hits$file[1] else NULL)))
  },
  get_macro_source = function(ctx) function(args) {
    nm <- tolower(args$name %||% "")
    if (is.null(ctx$macro_index) || !nrow(ctx$macro_index)) return(list(error = "not_in_index"))
    hits <- ctx$macro_index[ctx$macro_index$name == nm, ]
    if (!nrow(hits)) return(list(error = "not_in_index"))
    lines <- readLines(hits$file[1], warn = FALSE)
    span <- hits$line_start[1]:min(hits$line_end[1], hits$line_start[1] + 399L)
    list(source = paste(lines[span], collapse = "\n"),
         truncated = hits$line_end[1] > hits$line_start[1] + 399L)
  },
  list_macro_files = function(ctx) function(args) {
    if (is.null(ctx$macro_index) || !nrow(ctx$macro_index)) return(list(files = character()))
    list(files = unique(basename(ctx$macro_index$file)))
  },
  search_skills = function(ctx) function(args) {
    catalog <- ctx$skill_catalog %||% agent_skill_catalog()
    q <- args$query %||% ""
    list(skills = search_registered_skills(q, catalog))
  },
  read_skill = function(ctx) function(args) {
    catalog <- ctx$skill_catalog %||% agent_skill_catalog()
    nm <- args$name %||% ""
    tryCatch(
      read_registered_skill(nm, catalog),
      sas2r_skill_not_registered = function(e) list(error = "skill_not_registered", message = conditionMessage(e))
    )
  },
  read_comparison_report = function(ctx) function(args) {
    report_id <- args$report_id %||% ""
    registry <- ctx$report_registry %||% ctx$comparison_reports %||% new.env(parent = emptyenv())
    # A bounded tool answers the model with a typed record. Letting the
    # condition escape would abort the whole refinement run -- losing every
    # comparison report already computed -- because a model named a stale or
    # invented report_id. read_skill does the same.
    tryCatch(
      read_comparison_report(report_id, registry),
      # Most specific first: report_path_rejected inherits from
      # report_not_registered, so the parent handler would otherwise shadow it.
      sas2r_report_path_rejected = function(e) {
        list(error = "report_path_rejected", message = conditionMessage(e))
      },
      sas2r_report_not_registered = function(e) {
        list(error = "report_not_registered", message = conditionMessage(e))
      }
    )
  }
)

#' Build tools for an agent spec bound to execution context
#'
#' @param spec Agent spec.
#' @param ctx Execution context.
#' @return Named list of tool objects.
#' @noRd
build_tools <- function(spec, ctx) {
  out <- list()
  for (nm in names(spec$tools)) {
    if (nm == "search_docs") {
      if (isTRUE(ctx$config$search_docs$enabled) && !isFALSE(spec$tools$search_docs$enabled)) {
        if (exists("search_docs_impl", mode = "function")) {
          out[[nm]] <- make_tool(
            nm, search_docs_impl(ctx), spec$tools[[nm]]$max_calls %||% 3L,
            schema = tool_argument_schema(nm),
            description = unname(TOOL_DESCRIPTIONS[[nm]])
          )
        }
      }
      next
    }
    impl <- TOOL_IMPLS[[nm]]
    if (is.null(impl)) next
    out[[nm]] <- make_tool(
      nm, impl(ctx), spec$tools[[nm]]$max_calls %||% 3L,
      schema = tool_argument_schema(nm),
      description = unname(TOOL_DESCRIPTIONS[[nm]])
    )
  }
  out
}
