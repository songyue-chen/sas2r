test_that("installed package skills validate with stable hashes", {
  skills <- sas2r:::agent_skill_catalog()
  expect_setequal(names(skills),
                  c("sas-missing-sort-semantics",
                    "sas-dataset-row-alignment"))
  expect_true(all(vapply(skills, function(x)
    grepl("^[a-f0-9]{64}$", x$content_hash), logical(1))))
  expect_true(all(vapply(skills, function(x)
    identical(x$skill_id, x$folder_name), logical(1))))
  expect_true(all(vapply(skills, function(x)
    is.integer(x$version) && x$version >= 1L, logical(1))))
  expect_true(all(vapply(skills, function(x)
    is.character(x$description) && nzchar(x$description), logical(1))))
  expect_true(all(vapply(skills, function(x)
    is.character(x$body) && nzchar(x$body), logical(1))))
  expect_true(all(vapply(skills, function(x)
    is.character(x$agents) && length(x$agents) > 0L, logical(1))))
  expect_true(all(vapply(skills, function(x)
    is.numeric(x$priority) && length(x$priority) == 1L, logical(1))))
  expect_true(all(vapply(skills, function(x)
    is.list(x$triggers) && length(x$triggers) > 0L, logical(1))))
  expect_true(all(vapply(skills, function(x)
    is.character(x$tools) && length(x$tools) > 0L, logical(1))))
})

test_that("skill loader fails closed on malformed package assets", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "wrong-folder"))
  writeLines(c("---", "name: another-name",
               "description: Use when PROC SORT is translated.", "---",
               "# Body"), file.path(root, "wrong-folder", "SKILL.md"))
  expect_error(sas2r:::load_agent_skills(root),
               class = "sas2r_skill_schema_error")
})

test_that("production skill catalogue never searches a project directory", {
  project <- withr::local_tempdir()
  dir.create(file.path(project, ".sas2r", "skills", "project-skill"),
             recursive = TRUE)
  writeLines(c("---", "name: project-skill",
               "description: Use for every translation.", "---", "# Ignore"),
             file.path(project, ".sas2r", "skills", "project-skill", "SKILL.md"))
  withr::local_dir(project)

  # agent_skill_catalog() memoizes. Every other test in this file, and every
  # agent test in the suite, primes that memo first -- so by the time this
  # assertion ran it was reading a catalogue loaded before the working
  # directory existed, and passed without the loader ever being asked where to
  # look. Clear the memo so the load path itself is what is under test, and
  # restore it afterwards so the rest of the suite keeps its warm cache.
  memo <- environment(sas2r:::agent_skill_catalog)
  warm <- memo$cached
  memo$cached <- NULL
  withr::defer({
    memo$cached <- warm
  })

  catalog <- sas2r:::agent_skill_catalog()
  # The load path really ran and really produced the shipped catalogue: an
  # empty result would satisfy the exclusion below for the wrong reason.
  expect_gt(length(catalog), 0L)
  expect_identical(catalog, sas2r:::load_agent_skills(
    system.file("skills", package = "sas2r")))
  expect_false("project-skill" %in% names(catalog))
})

test_that("skill loader validates root existence and scalar character", {
  expect_error(sas2r:::load_agent_skills(NULL),
               class = "sas2r_skill_schema_error")
  expect_error(sas2r:::load_agent_skills(c("a", "b")),
               class = "sas2r_skill_schema_error")
  expect_error(sas2r:::load_agent_skills("/nonexistent/path/for/skills"),
               class = "sas2r_skill_schema_error")
})

test_that("skill loader fails when SKILL.md is missing from subfolder", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "empty-skill"))
  expect_error(sas2r:::load_agent_skills(root),
               class = "sas2r_skill_schema_error")
})

test_that("skill loader fails on missing or malformed frontmatter delimiters", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "bad-delim"))
  writeLines(c("name: bad-delim", "description: test", "---", "# Body"),
             file.path(root, "bad-delim", "SKILL.md"))
  expect_error(sas2r:::load_agent_skills(root),
               class = "sas2r_skill_schema_error")

  root2 <- withr::local_tempdir()
  dir.create(file.path(root2, "unclosed-delim"))
  writeLines(c("---", "name: unclosed-delim", "description: test", "# Body"),
             file.path(root2, "unclosed-delim", "SKILL.md"))
  expect_error(sas2r:::load_agent_skills(root2),
               class = "sas2r_skill_schema_error")
})

test_that("skill loader fails on invalid YAML syntax", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "bad-yaml"))
  writeLines(c("---", "name: [bad yaml: {", "---", "# Body"),
             file.path(root, "bad-yaml", "SKILL.md"))
  expect_error(sas2r:::load_agent_skills(root),
               class = "sas2r_skill_schema_error")
})

test_that("skill loader enforces name patterns and non-empty description", {
  # Uppercase in name
  root <- withr::local_tempdir()
  dir.create(file.path(root, "Invalid_Name"))
  writeLines(c(
    "---",
    "name: Invalid_Name",
    "description: Valid description.",
    "metadata:",
    "  sas2r:",
    "    version: 1",
    "    agents: [translator]",
    "    priority: 100",
    "    triggers:",
    "      procs: [sort]",
    "    tools: [lookup_rulebook]",
    "---",
    "# Body"
  ), file.path(root, "Invalid_Name", "SKILL.md"))
  expect_error(sas2r:::load_agent_skills(root),
               class = "sas2r_skill_schema_error")

  # Empty description
  root2 <- withr::local_tempdir()
  dir.create(file.path(root2, "empty-desc"))
  writeLines(c(
    "---",
    "name: empty-desc",
    "description: \"\"",
    "metadata:",
    "  sas2r:",
    "    version: 1",
    "    agents: [translator]",
    "    priority: 100",
    "    triggers:",
    "      procs: [sort]",
    "    tools: [lookup_rulebook]",
    "---",
    "# Body"
  ), file.path(root2, "empty-desc", "SKILL.md"))
  expect_error(sas2r:::load_agent_skills(root2),
               class = "sas2r_skill_schema_error")
})

test_that("skill loader validates version, agents, tools, and triggers", {
  # Nonpositive version
  root <- withr::local_tempdir()
  dir.create(file.path(root, "bad-version"))
  writeLines(c(
    "---",
    "name: bad-version",
    "description: Valid description.",
    "metadata:",
    "  sas2r:",
    "    version: 0",
    "    agents: [translator]",
    "    priority: 100",
    "    triggers:",
    "      procs: [sort]",
    "    tools: [lookup_rulebook]",
    "---",
    "# Body"
  ), file.path(root, "bad-version", "SKILL.md"))
  expect_error(sas2r:::load_agent_skills(root),
               class = "sas2r_skill_schema_error")

  # Unsupported agent
  root2 <- withr::local_tempdir()
  dir.create(file.path(root2, "bad-agent"))
  writeLines(c(
    "---",
    "name: bad-agent",
    "description: Valid description.",
    "metadata:",
    "  sas2r:",
    "    version: 1",
    "    agents: [unknown_agent]",
    "    priority: 100",
    "    triggers:",
    "      procs: [sort]",
    "    tools: [lookup_rulebook]",
    "---",
    "# Body"
  ), file.path(root2, "bad-agent", "SKILL.md"))
  expect_error(sas2r:::load_agent_skills(root2),
               class = "sas2r_skill_schema_error")

  # Unknown tool
  root3 <- withr::local_tempdir()
  dir.create(file.path(root3, "bad-tool"))
  writeLines(c(
    "---",
    "name: bad-tool",
    "description: Valid description.",
    "metadata:",
    "  sas2r:",
    "    version: 1",
    "    agents: [translator]",
    "    priority: 100",
    "    triggers:",
    "      procs: [sort]",
    "    tools: [nonexistent_tool]",
    "---",
    "# Body"
  ), file.path(root3, "bad-tool", "SKILL.md"))
  expect_error(sas2r:::load_agent_skills(root3),
               class = "sas2r_skill_schema_error")

  # Unknown trigger key
  root4 <- withr::local_tempdir()
  dir.create(file.path(root4, "bad-trigger-key"))
  writeLines(c(
    "---",
    "name: bad-trigger-key",
    "description: Valid description.",
    "metadata:",
    "  sas2r:",
    "    version: 1",
    "    agents: [translator]",
    "    priority: 100",
    "    triggers:",
    "      unknown_key: [foo]",
    "    tools: [lookup_rulebook]",
    "---",
    "# Body"
  ), file.path(root4, "bad-trigger-key", "SKILL.md"))
  expect_error(sas2r:::load_agent_skills(root4),
               class = "sas2r_skill_schema_error")

  # Nonexistent semantic rule trigger
  root5 <- withr::local_tempdir()
  dir.create(file.path(root5, "bad-semantic-rule"))
  writeLines(c(
    "---",
    "name: bad-semantic-rule",
    "description: Valid description.",
    "metadata:",
    "  sas2r:",
    "    version: 1",
    "    agents: [translator]",
    "    priority: 100",
    "    triggers:",
    "      semantic_rules: [procs.nonexistent_proc]",
    "    tools: [lookup_rulebook]",
    "---",
    "# Body"
  ), file.path(root5, "bad-semantic-rule", "SKILL.md"))
  expect_error(sas2r:::load_agent_skills(root5),
               class = "sas2r_skill_schema_error")
})

test_that("skill loader rejects duplicate fields and duplicate entries", {
  # Duplicate agents
  root <- withr::local_tempdir()
  dir.create(file.path(root, "dup-agent"))
  writeLines(c(
    "---",
    "name: dup-agent",
    "description: Valid description.",
    "metadata:",
    "  sas2r:",
    "    version: 1",
    "    agents: [translator, translator]",
    "    priority: 100",
    "    triggers:",
    "      procs: [sort]",
    "    tools: [lookup_rulebook]",
    "---",
    "# Body"
  ), file.path(root, "dup-agent", "SKILL.md"))
  expect_error(sas2r:::load_agent_skills(root),
               class = "sas2r_skill_schema_error")

  # Duplicate tools
  root2 <- withr::local_tempdir()
  dir.create(file.path(root2, "dup-tool"))
  writeLines(c(
    "---",
    "name: dup-tool",
    "description: Valid description.",
    "metadata:",
    "  sas2r:",
    "    version: 1",
    "    agents: [translator]",
    "    priority: 100",
    "    triggers:",
    "      procs: [sort]",
    "    tools: [lookup_rulebook, lookup_rulebook]",
    "---",
    "# Body"
  ), file.path(root2, "dup-tool", "SKILL.md"))
  expect_error(sas2r:::load_agent_skills(root2),
               class = "sas2r_skill_schema_error")

  # Duplicate YAML top-level keys
  root3 <- withr::local_tempdir()
  dir.create(file.path(root3, "dup-keys"))
  writeLines(c(
    "---",
    "name: dup-keys",
    "name: dup-keys",
    "description: Valid description.",
    "metadata:",
    "  sas2r:",
    "    version: 1",
    "    agents: [translator]",
    "    priority: 100",
    "    triggers:",
    "      procs: [sort]",
    "    tools: [lookup_rulebook]",
    "---",
    "# Body"
  ), file.path(root3, "dup-keys", "SKILL.md"))
  expect_error(sas2r:::load_agent_skills(root3),
               class = "sas2r_skill_schema_error")
})

test_that("validate_agent_skill validates valid and invalid skill records", {
  valid_skill <- list(
    skill_id = "sas-missing-sort-semantics",
    folder_name = "sas-missing-sort-semantics",
    version = 1L,
    description = "A valid skill description.",
    body = "# Body content",
    content_hash = paste(rep("a", 64), collapse = ""),
    agents = c("translator", "reviewer"),
    priority = 100L,
    triggers = list(
      semantic_rules = "procs.sort",
      procs = "sort"
    ),
    tools = c("lookup_rulebook", "query_project_graph")
  )

  expect_silent(sas2r:::validate_agent_skill(valid_skill))

  # Missing field
  invalid_skill <- valid_skill
  invalid_skill$version <- NULL
  expect_error(sas2r:::validate_agent_skill(invalid_skill),
               class = "sas2r_skill_schema_error")

  # Invalid content hash
  invalid_hash <- valid_skill
  invalid_hash$content_hash <- "short-hash"
  expect_error(sas2r:::validate_agent_skill(invalid_hash),
               class = "sas2r_skill_schema_error")

  # Non-list input
  expect_error(sas2r:::validate_agent_skill("not a skill"),
               class = "sas2r_skill_schema_error")

  # Unknown trigger key
  invalid_trig <- valid_skill
  invalid_trig$triggers$unknown_key <- "foo"
  expect_error(sas2r:::validate_agent_skill(invalid_trig),
               class = "sas2r_skill_schema_error")

  # Unknown tools
  invalid_tool <- valid_skill
  invalid_tool$tools <- "nonexistent_tool"
  expect_error(sas2r:::validate_agent_skill(invalid_tool),
               class = "sas2r_skill_schema_error")

  # Unsupported agent
  invalid_agent <- valid_skill
  invalid_agent$agents <- "unknown_agent"
  expect_error(sas2r:::validate_agent_skill(invalid_agent),
               class = "sas2r_skill_schema_error")

  # Nonexistent semantic rule
  invalid_sem <- valid_skill
  invalid_sem$triggers$semantic_rules <- "procs.nonexistent"
  expect_error(sas2r:::validate_agent_skill(invalid_sem),
               class = "sas2r_skill_schema_error")
})

test_that("curated skill files contain required domain concepts", {
  skills <- sas2r:::agent_skill_catalog()

  # sas-missing-sort-semantics
  sort_skill <- skills[["sas-missing-sort-semantics"]]
  expect_false(is.null(sort_skill))
  expect_match(sort_skill$body, "\\._", all = FALSE)
  expect_match(sort_skill$body, "\\.A", all = FALSE)
  expect_match(sort_skill$body, "\\.Z", all = FALSE)
  expect_match(sort_skill$body, "PROC SORT", all = FALSE)
  expect_match(sort_skill$body, "FIRST\\.", all = FALSE)
  expect_match(sort_skill$body, "LAST\\.", all = FALSE)
  expect_match(sort_skill$body, "BY", all = FALSE)
  expect_match(sort_skill$body, "collation", ignore.case = TRUE, all = FALSE)
  expect_match(sort_skill$body, "NA", all = FALSE)

  # sas-dataset-row-alignment
  align_skill <- skills[["sas-dataset-row-alignment"]]
  expect_false(is.null(align_skill))
  expect_match(align_skill$body, "position", ignore.case = TRUE, all = FALSE)
  expect_match(align_skill$body, "multiset", ignore.case = TRUE, all = FALSE)
  expect_match(align_skill$body, "unique", ignore.case = TRUE, all = FALSE)
  expect_match(align_skill$body, "order", ignore.case = TRUE, all = FALSE)
})

test_that("sort metadata deterministically routes missing-sort guidance", {
  ctx <- list(agent = "reviewer", unit_type = "proc_step",
              semantic_rules = "procs.sort", procs = "sort",
              flags = c("by_group", "order_dependent"),
              comparison_reasons = character(), macros = character(),
              functions = character())
  routed <- sas2r:::route_agent_skills(ctx)
  expect_identical(vapply(routed, `[[`, "", "skill_id"),
                   c("sas-missing-sort-semantics", "sas-dataset-row-alignment"))
  # The activation reason is fully determined by the matched trigger values, so
  # pin it exactly: an alternation like "procs.sort|sort" is satisfied by "sort"
  # alone and therefore cannot witness the semantic_rules trigger firing.
  expect_identical(routed[[1]]$activation_reason,
                   "procs.sort,sort,by_group,order_dependent")
  expect_identical(routed[[2]]$activation_reason, "sort,order_dependent")
})

test_that("skill tools accept names and queries but never paths", {
  catalog <- sas2r:::agent_skill_catalog()
  expect_true(length(sas2r:::search_registered_skills("missing sort", catalog)) > 0L)
  # Path-shaped input must be rejected *as a path*. Asserting only the shared
  # parent class would be satisfied by the ordinary not-in-catalogue branch.
  expect_error(sas2r:::read_registered_skill("../SKILL.md", catalog),
               class = "sas2r_skill_path_rejected")
})

test_that("routing a skill spends no model request", {
  calls <- 0L
  llm <- sas2r:::new_llm(function(request) {
    calls <<- calls + 1L
    stop("transport must not be called")
  }, provider = "mock")
  invisible(sas2r:::route_agent_skills(list(
    agent = "translator", unit_type = "proc_step", procs = "sort",
    semantic_rules = "procs.sort", flags = character(),
    comparison_reasons = character(), macros = character(),
    functions = character())))
  expect_identical(calls, 0L)
})

test_that("route_agent_skills orders skills by priority, score, and radix name", {
  catalog <- list(
    `skill-b` = list(
      skill_id = "skill-b",
      version = 1L,
      content_hash = paste(rep("b", 64), collapse = ""),
      agents = c("translator"),
      priority = 50L,
      triggers = list(procs = c("sort")),
      body = "Body B"
    ),
    `skill-a` = list(
      skill_id = "skill-a",
      version = 1L,
      content_hash = paste(rep("a", 64), collapse = ""),
      agents = c("translator"),
      priority = 100L,
      triggers = list(procs = c("sort")),
      body = "Body A"
    ),
    `skill-c` = list(
      skill_id = "skill-c",
      version = 1L,
      content_hash = paste(rep("c", 64), collapse = ""),
      agents = c("translator"),
      priority = 100L,
      triggers = list(procs = c("sort"), flags = c("by_group")),
      body = "Body C"
    )
  )

  ctx <- list(agent = "translator", procs = "sort", flags = "by_group")
  routed <- sas2r:::route_agent_skills(ctx, catalog = catalog)
  # skill-c: priority 100, score 2
  # skill-a: priority 100, score 1
  # skill-b: priority 50, score 1
  expect_identical(vapply(routed, `[[`, "", "skill_id"),
                   c("skill-c", "skill-a", "skill-b"))
})

test_that("render_agent_skills renders formatted markdown or fallback", {
  expect_identical(sas2r:::render_agent_skills(list()), "No routed package skills.")
  records <- list(
    list(
      skill_id = "sas-missing-sort-semantics",
      version = 1L,
      activation_reason = "procs.sort,sort",
      body = "# Content here"
    )
  )
  rendered <- sas2r:::render_agent_skills(records)
  expect_match(rendered, "## Skill: sas-missing-sort-semantics v1")
  expect_match(rendered, "Activation: procs.sort,sort")
  expect_match(rendered, "# Content here")
})

test_that("search_registered_skills caps at 10 records and returns name and description", {
  catalog <- sas2r:::agent_skill_catalog()
  res <- sas2r:::search_registered_skills("sort", catalog)
  expect_true(length(res) >= 1L)
  expect_named(res[[1]], c("name", "description"), ignore.order = TRUE)

  # The packaged catalogue is smaller than the cap, so it cannot exercise it.
  # Build an oversized catalogue where every entry matches the query.
  wide <- stats::setNames(lapply(seq_len(25L), function(i) {
    list(
      skill_id = sprintf("sas-sort-skill-%02d", i),
      version = 1L,
      content_hash = strrep(letters[(i %% 6L) + 1L], 64L),
      description = "sort guidance",
      agents = "translator",
      priority = as.integer(i),
      triggers = list(procs = "sort"),
      body = "body"
    )
  }), sprintf("sas-sort-skill-%02d", seq_len(25L)))
  expect_gt(length(wide), 10L)

  wide_res <- sas2r:::search_registered_skills("sort", wide)
  expect_length(wide_res, 10L)
  expect_true(all(vapply(wide_res, function(x) identical(names(x), c("name", "description")),
                         logical(1))))
  # Cap keeps the top-ranked records (highest priority first), not an arbitrary slice.
  expect_identical(vapply(wide_res, `[[`, "", "name"),
                   sprintf("sas-sort-skill-%02d", 25L:16L))

  # Non-matching query returns empty list
  empty_res <- sas2r:::search_registered_skills("nonexistent_term_xyz", catalog)
  expect_identical(empty_res, list())
})

test_that("read_registered_skill returns full skill record and errors on uncurated names", {
  catalog <- sas2r:::agent_skill_catalog()
  skill <- sas2r:::read_registered_skill("sas-missing-sort-semantics", catalog)
  expect_identical(skill$skill_id, "sas-missing-sort-semantics")
  expect_identical(skill$version, 1L)
  expect_true(grepl("^[a-f0-9]{64}$", skill$content_hash))
  expect_match(skill$body, "PROC SORT")

  # Fails on invalid names, and rejects directory traversal *as traversal*.
  # A plain unknown name must NOT be reported as a path rejection, otherwise
  # the traversal guard could be deleted without any test noticing.
  unknown <- expect_error(sas2r:::read_registered_skill("unknown-skill", catalog),
                          class = "sas2r_skill_not_registered")
  expect_false(inherits(unknown, "sas2r_skill_path_rejected"))

  for (bad in c("/etc/passwd", "..", "../SKILL.md", "..\\SKILL.md",
                "sas-missing-sort-semantics/SKILL.md")) {
    err <- expect_error(sas2r:::read_registered_skill(bad, catalog),
                        class = "sas2r_skill_path_rejected")
    # The subclass keeps the established parent class for existing handlers.
    expect_s3_class(err, "sas2r_skill_not_registered")
  }
})

test_that("skill flags derive from SAS source order semantics", {
  expect_setequal(skill_flags_from_sas("if first.usubjid then n = 1;"),
                  c("by_group", "order_dependent"))
  expect_identical(skill_flags_from_sas("retain c 0;"), "order_dependent")
  expect_identical(skill_flags_from_sas("proc sort data=a; by k; run;"),
                   "order_dependent")
  expect_identical(skill_flags_from_sas("x = 1;"), character(0))
})

test_that("comparison reasons derive from repair difference evidence", {
  outputs <- list(list(
    target_key = "adam.adsl", kind = "dataset", status = "failed",
    differences = list(
      digest = list(pattern_hints = data.frame(
        var = c("aval", NA), hint = c("NA_PATTERN_DIFF", "DUPLICATE_KEYS"),
        stringsAsFactors = FALSE)),
      structure = list(alignment_method = "keyless_multiset", dup_fanout = FALSE)
    )
  ))
  expect_setequal(
    comparison_reasons_from_outputs(outputs),
    c("missing_order_difference", "duplicate_key_order", "keyless_reorder")
  )
  expect_identical(comparison_reasons_from_outputs(list()), character(0))
  expect_identical(comparison_reasons_from_outputs(NULL), character(0))
})

test_that("ordering skills route for translator components with retain or by-group logic", {
  dir <- withr::local_tempdir()
  writeLines("data adam.out; set adam.adsl; retain c 0; run;",
             file.path(dir, "prog.sas"))
  proj <- sas_project(dir)
  baseline <- sas_transpile(proj, withr::local_tempdir())
  graph <- build_dependency_graph(proj)
  ctx <- build_translator_context("prog", proj, baseline = baseline, graph = graph)
  expect_match(ctx$rendered_skills, "sas-missing-sort-semantics")
})

test_that("the fixer gets routed skills, the allowlist, working macro tools, and the comparison report", {
  dir <- withr::local_tempdir()
  macro_dir <- file.path(dir, "macros")
  dir.create(macro_dir)
  writeLines(c("%macro dostuff(x);", "%mend;"), file.path(macro_dir, "dostuff.sas"))
  sas <- "data adam.out; set adam.adsl; run;"
  writeLines(sas, file.path(dir, "prog.sas"))
  cfg <- list(macro_search_path = "macros")
  proj <- sas_project(dir, config = cfg)

  registry <- new.env(parent = emptyenv())
  ref <- tibble::tibble(USUBJID = c("1", "1"), AVAL = c(10, 20))
  cand <- tibble::tibble(USUBJID = c("1", "1"), AVAL = c(20, 10))
  report <- compare_aligned_outputs(ref, cand, target = list(
    target_id = "adam.out", logical_dataset = "adam.out", role = "output",
    contributing_unit_ids = integer()))
  assign(report$report_id, report, envir = registry)

  outputs_ev <- list(list(
    target_key = "adam.out", kind = "dataset", status = "failed",
    checks = list(),
    differences = list(
      structure = list(alignment_method = "duplicate_key_multiset",
                       dup_fanout = TRUE),
      digest = list(pattern_hints = data.frame(
        var = "aval", hint = "DUPLICATE_KEYS", stringsAsFactors = FALSE)),
      comparison_report_id = report$report_id
    )
  ))

  captured <- list()
  llm <- recording_fixer(function(context) {
    captured[[length(captured) + 1L]] <<- context
    valid_program_fix_response(code = "x <- 1", evidence_ids = "ev1")
  })
  revision <- list(component_id = "prog", revision_id = "r1", r_code = "bad",
                   contract = list(component_id = "prog", sas_text = sas))
  res <- fix_program_revision(
    revision = revision, outputs = outputs_ev, mode = "bundle",
    llm = llm, paths = init_migration_paths(withr::local_tempdir()),
    project = proj, config = cfg, evidence_ids = "ev1",
    report_registry = registry
  )

  req <- captured[[1L]]$request
  sys_txt <- req$messages[[1L]]$content
  # Routed via the mapped comparison reasons.
  expect_match(sys_txt, "sas-dataset-row-alignment")
  # The allowlist reaches the prompt (fixer default).
  expect_match(sys_txt, "dplyr, tidyr, haven")
  # The report id is named in the evidence so the model can request it.
  expect_match(sys_txt, report$report_id, fixed = TRUE)

  find_tool <- function(nm) {
    for (t in req$tools) if (identical(t$name, nm)) return(t)
    NULL
  }
  ans <- find_tool("find_macro")$call(list(name = "dostuff"))
  expect_null(ans$error)
  got <- find_tool("read_comparison_report")$call(list(report_id = report$report_id))
  expect_identical(got$report_id, report$report_id)
  unit <- find_tool("read_unit_context")$call(list())
  expect_match(unit$sas, "adam.adsl", fixed = TRUE)
})
