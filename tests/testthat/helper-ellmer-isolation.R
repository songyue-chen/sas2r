run_isolated_ellmer_s7 <- function(fixture_pkg) {
  lib <- tempfile("sas2r-ellmer-lib-")
  dir.create(lib)
  status <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", paste0("--library=", lib), fixture_pkg)
  )
  testthat::expect_identical(status, 0L)

  package_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  # Same rule as the acceptance gate: key on how this process loaded sas2r,
  # not on what is on disk. An unpacked tarball still carries R/llm.R, so a
  # filesystem probe would load the source in the installed-package jobs.
  source_available <- requireNamespace("pkgload", quietly = TRUE) &&
    isTRUE(pkgload::is_dev_package("sas2r")) &&
    file.exists(file.path(package_root, "R", "llm.R"))
  callr::r(
    function(lib, package_root, source_available) {
      .libPaths(c(lib, .libPaths()))
      lib <- normalizePath(lib, mustWork = TRUE)
      ellmer_path <- normalizePath(find.package("ellmer"), mustWork = TRUE)
      stopifnot(startsWith(ellmer_path, lib))
      if (source_available) {
        pkgload::load_all(package_root, quiet = TRUE)
      } else {
        loadNamespace("sas2r")
      }
      ellmer::reset_contract_log()
      capabilities <- list(
        structured_output = "native",
        tool_calling = "native",
        tools_with_structured_output = "supported",
        max_output_tokens = "supported"
      )
      adapter <- sas2r:::ellmer_llm(list(
        provider = "azure", model = "s7-model",
        endpoint = "https://azure.example.invalid",
        api_version = "2026-08-20",
        capabilities = capabilities
      ))
      response <- adapter$request(sas2r:::llm_request(
        messages = list(
          list(role = "system", content = "policy"),
          list(role = "user", content = "question"),
          list(
            role = "assistant", content = "",
            tool_call = list(
              id = "call_1", name = "echo", arguments = list(x = 1)
            )
          ),
          list(
            role = "tool", name = "echo", tool_call_id = "call_1",
            content = '{"x":1}'
          ),
          list(role = "user", content = "final answer")
        ),
        output_schema = list(
          type = "object",
          properties = list(r_code = list(type = "string")),
          required = "r_code",
          additionalProperties = FALSE
        ),
        schema_name = "fixture", schema_version = "1",
        schema_mode = "native", max_output_tokens = 321L
      ))

      provider_configs <- list(
        list(
          provider = "openai", model = "s7-openai",
          base_url = "https://openai.example.invalid/v1",
          capabilities = capabilities
        ),
        list(
          provider = "ollama", model = "s7-ollama",
          base_url = "http://ollama.example.invalid",
          capabilities = capabilities
        ),
        list(
          provider = "bedrock", model = "s7-bedrock",
          region = "us-west-2", profile = "research", cache = "auto",
          capabilities = capabilities
        ),
        list(
          provider = "vertex", model = "s7-vertex",
          project_id = "project-a", location = "us-central1",
          capabilities = capabilities
        ),
        list(
          provider = "anthropic", model = "s7-anthropic",
          base_url = "https://anthropic.example.invalid/v1", cache = "1h",
          capabilities = capabilities
        ),
        list(
          provider = "databricks", model = "s7-databricks",
          workspace = "https://example.cloud.databricks.invalid",
          capabilities = capabilities
        ),
        list(
          provider = "deepseek", model = "s7-deepseek",
          base_url = "https://deepseek.example.invalid",
          capabilities = capabilities
        ),
        list(
          provider = "github", model = "s7-github",
          base_url = "https://models.example.invalid/inference/",
          models_base_url = "https://models.example.invalid/",
          capabilities = capabilities
        ),
        list(
          provider = "gemini", model = "s7-gemini",
          credentials = function() "s7-gemini-token",
          capabilities = capabilities
        ),
        list(
          provider = "posit", model = "s7-posit", cache = "none",
          credentials = function() "s7-posit-token",
          capabilities = capabilities
        ),
        list(
          provider = "snowflake", model = "s7-snowflake",
          account = "org-acct", credentials = function() "s7-snowflake-token",
          capabilities = capabilities
        )
      )
      provider_effective_coexistence <- character()
      provider_effective_coexistence[["azure"]] <-
        sas2r:::llm_capabilities_for(adapter)$tools_with_structured_output
      for (config in provider_configs) {
        provider_adapter <- sas2r:::ellmer_llm(config)
        provider_effective_coexistence[[config$provider]] <-
          sas2r:::llm_capabilities_for(
            provider_adapter
          )$tools_with_structured_output
        provider_adapter$request(sas2r:::llm_request(
          messages = list(list(role = "user", content = "provider selector")),
          output_schema = list(
            type = "object",
            properties = list(r_code = list(type = "string")),
            required = "r_code", additionalProperties = FALSE
          ),
          schema_name = "fixture", schema_version = "1",
          schema_mode = "native"
        ))
      }
      bedrock_inventory <- sas2r::sas_llm_models(provider_configs[[3]])
      vertex_inventory <- sas2r::sas_llm_models(provider_configs[[4]])
      named_configs <- stats::setNames(
        provider_configs, vapply(provider_configs, `[[`, "", "provider")
      )
      agreed_inventories <- lapply(
        named_configs[c(
          "openai", "ollama", "anthropic", "databricks", "deepseek", "github",
          "gemini", "posit", "snowflake"
        )],
        sas2r::sas_llm_models
      )
      # Every inventory the registry declares has to resolve against the
      # isolated fixture. Without this a typo or a drift in `models_export`
      # reaches users as "this provider has no inventory endpoint" instead of
      # failing the contract.
      declared_inventories <- Filter(Negate(is.null), lapply(
        sas2r:::llm_provider_registry(), `[[`, "models_export"
      ))
      inventory_exports_resolved <- vapply(declared_inventories, function(export) {
        exists(export, asNamespace("ellmer"), mode = "function", inherits = FALSE)
      }, logical(1))
      github_custom_inventory <- sas2r::sas_llm_models(list(
        provider = "github", model = "s7-github",
        base_url = "https://models.example.invalid/inference/"
      ))
      deepseek_default_capabilities <- sas2r:::ellmer_llm(list(
        provider = "deepseek", model = "s7-deepseek",
        base_url = "https://deepseek.example.invalid"
      ))$capabilities

      two_phase_executions <- 0L
      lookup <- sas2r:::make_tool(
        "lookup", function(args) {
          two_phase_executions <<- two_phase_executions + 1L
          list(name = args$name)
        }, max_calls = 10L,
        schema = list(
          type = "object",
          properties = list(
            name = list(type = "string"),
            functions = list(type = "array", items = list(type = "string")),
            operators = list(type = "array", items = list(type = "string")),
            procs = list(type = "array", items = list(type = "string"))
          ),
          required = "name", additionalProperties = FALSE
        )
      )
      spec <- list(
        name = "fixture", prompt = "translator.md", tier = "frontier",
        tool_call_limit = 2L, retry_limit = 0L, temperature = NULL,
        output_schema = "program_translation_v1"
      )
      two_phase <- sas2r:::run_agent(
        spec, adapter, list(lookup = lookup), "translate",
        log_dir = tempfile("sas2r-ellmer-log-")
      )

      executions <- 0L
      limited_tool <- sas2r:::make_tool(
        "echo", function(args) {
          executions <<- executions + 1L
          list(x = args$x)
        }, max_calls = 10L,
        schema = list(
          type = "object", properties = list(x = list(type = "number")),
          required = "x", additionalProperties = FALSE
        )
      )
      limited_adapter <- sas2r:::ellmer_llm(list(
        provider = "azure", model = "s7-tool-limit",
        endpoint = "https://azure.example.invalid",
        api_version = "2026-08-20",
        capabilities = capabilities
      ))
      limited_spec <- spec
      limited_spec$tool_call_limit <- 1L
      limited <- sas2r:::run_agent(
        limited_spec, limited_adapter, list(echo = limited_tool), "translate",
        log_dir = tempfile("sas2r-ellmer-limit-log-")
      )

      configured_secret <- "fixture-configured-key-843e"
      secret_log_dir <- tempfile("sas2r-ellmer-secret-log-")
      secret_adapter <- sas2r:::ellmer_llm(list(
        provider = "azure", auth_mode = "api_key", model = "s7-secret-error",
        endpoint = "https://azure.example.invalid",
        api_version = "2026-08-20", api_key = configured_secret,
        capabilities = capabilities
      ))
      secret_result <- sas2r:::run_agent(
        spec, secret_adapter, list(), "translate", log_dir = secret_log_dir
      )
      secret_log <- paste(readLines(
        file.path(secret_log_dir, "llm_log.jsonl"), warn = FALSE
      ), collapse = "\n")

      # `workspace`/`account` omitted: sas2r passes no such argument, so the
      # fixture's mirror of ellmer's own `databricks_workspace()` /
      # `snowflake_account()` default resolves the tenant from its documented
      # environment variable. No credential file or CLI profile is consulted.
      old_ambient <- Sys.getenv(
        c("DATABRICKS_HOST", "SNOWFLAKE_ACCOUNT"), unset = NA_character_
      )
      Sys.setenv(
        DATABRICKS_HOST = "ambient.databricks.invalid",
        SNOWFLAKE_ACCOUNT = "ambient-acct"
      )
      on.exit({
        for (name in names(old_ambient)) {
          if (is.na(old_ambient[[name]])) {
            Sys.unsetenv(name)
          } else {
            do.call(Sys.setenv, stats::setNames(list(old_ambient[[name]]), name))
          }
        }
      }, add = TRUE)
      ambient_identities <- list()
      for (config in list(
        list(provider = "databricks", model = "s7-databricks",
             capabilities = capabilities),
        list(provider = "snowflake", model = "s7-snowflake",
             capabilities = capabilities)
      )) {
        ambient_adapter <- sas2r:::ellmer_llm(config)
        ambient_adapter$request(sas2r:::llm_request(
          messages = list(list(role = "user", content = "ambient tenant")),
          output_schema = list(
            type = "object",
            properties = list(r_code = list(type = "string")),
            required = "r_code", additionalProperties = FALSE
          ),
          schema_name = "fixture", schema_version = "1",
          schema_mode = "native"
        ))
        ambient_identities[[config$provider]] <- ambient_adapter$endpoint
      }
      ambient_constructors <- utils::tail(
        ellmer::get_contract_log()$constructors, 2L
      )

      list(
        ellmer_path = ellmer_path,
        direct_response = response,
        code = response$data$r_code,
        usage = response$usage,
        cost = response$cost$amount_usd,
        finish_reason = response$finish_reason,
        two_phase = two_phase,
        two_phase_executions = two_phase_executions,
        limited = limited,
        limited_executions = executions,
        secret_result = secret_result,
        configured_secret = configured_secret,
        secret_log = secret_log,
        bedrock_inventory = bedrock_inventory,
        vertex_inventory = vertex_inventory,
        agreed_inventories = agreed_inventories,
        declared_inventories = unlist(declared_inventories, use.names = FALSE),
        inventory_exports_resolved = inventory_exports_resolved,
        github_custom_inventory = github_custom_inventory,
        deepseek_default_capabilities = deepseek_default_capabilities,
        provider_effective_coexistence = provider_effective_coexistence,
        ambient_identities = ambient_identities,
        ambient_constructors = ambient_constructors,
        contract = ellmer::get_contract_log()
      )
    },
    args = list(
      lib = lib,
      package_root = package_root,
      source_available = source_available
    ),
    libpath = c(lib, .libPaths())
  )
}
