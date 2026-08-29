TypeJsonSchema <- S7::new_class(
  "TypeJsonSchema",
  properties = list(
    text = S7::class_character,
    required = S7::new_property(S7::class_logical, default = TRUE)
  )
)

ContentText <- S7::new_class(
  "ContentText",
  properties = list(text = S7::class_character)
)

ContentToolRequest <- S7::new_class(
  "ContentToolRequest",
  properties = list(
    id = S7::class_character,
    name = S7::class_character,
    arguments = S7::class_list
  )
)

ContentToolResult <- S7::new_class(
  "ContentToolResult",
  properties = list(
    request = ContentToolRequest,
    value = S7::class_any
  )
)

SystemTurn <- S7::new_class(
  "SystemTurn",
  properties = list(contents = S7::class_list)
)

UserTurn <- S7::new_class(
  "UserTurn",
  properties = list(contents = S7::class_list)
)

AssistantTurn <- S7::new_class(
  "AssistantTurn",
  properties = list(
    contents = S7::class_list,
    finish_reason = S7::class_character
  )
)

.contract_log <- new.env(parent = emptyenv())
.contract_log$constructors <- list()
.contract_log$calls <- list()
.contract_log$tools <- list()
.contract_log$inventories <- list()

reset_contract_log <- function() {
  .contract_log$constructors <- list()
  .contract_log$calls <- list()
  .contract_log$tools <- list()
  .contract_log$inventories <- list()
  invisible(NULL)
}

get_contract_log <- function() {
  list(
    constructors = .contract_log$constructors,
    calls = .contract_log$calls,
    tools = .contract_log$tools,
    inventories = .contract_log$inventories
  )
}

params <- function(...) list(...)

type_from_schema <- function(text) {
  stopifnot(is.character(text), length(text) == 1L, nzchar(text))
  if (!grepl("^\\s*\\{", text)) stop("schema must be serialized JSON text")
  TypeJsonSchema(text = text)
}

tool <- function(fun, name = NULL, description = NULL, arguments = NULL, ...) {
  formal_required <- vapply(
    formals(fun), function(value) identical(value, quote(expr = )), logical(1)
  )
  advertised_required <- vapply(
    arguments, function(argument) S7::prop(argument, "required"), logical(1)
  )
  stopifnot(
    is.function(fun), is.list(arguments),
    setequal(names(arguments), names(formals(fun))),
    all(vapply(
      arguments, function(argument) S7::S7_inherits(argument, TypeJsonSchema),
      logical(1)
    )),
    identical(formal_required[names(arguments)], advertised_required)
  )
  structure(
    list(
      fun = fun, name = name, description = description,
      arguments = arguments, required = advertised_required, extra = list(...)
    ),
    class = "ellmer_tool"
  )
}

contents_text <- function(turn) {
  contents <- S7::prop(turn, "contents")
  text <- vapply(contents, function(content) {
    if (S7::S7_inherits(content, ContentText)) S7::prop(content, "text") else ""
  }, character(1))
  paste(text[nzchar(text)], collapse = "\n")
}

turn_summary <- function(turn) {
  request_id <- NA_character_
  role <- if (S7::S7_inherits(turn, SystemTurn)) {
    "system"
  } else if (S7::S7_inherits(turn, UserTurn)) {
    contents <- S7::prop(turn, "contents")
    if (length(contents) && S7::S7_inherits(contents[[1]], ContentToolResult)) {
      request <- S7::prop(contents[[1]], "request")
      request_id <- S7::prop(request, "id")
      "tool"
    } else "user"
  } else if (S7::S7_inherits(turn, AssistantTurn)) {
    contents <- S7::prop(turn, "contents")
    if (length(contents) && S7::S7_inherits(contents[[1]], ContentToolRequest)) {
      request_id <- S7::prop(contents[[1]], "id")
      "assistant_tool"
    } else "assistant"
  } else stop("non-public turn object")
  list(role = role, text = contents_text(turn), request_id = request_id)
}

make_chat <- function(provider, model, params = NULL, base_url = NULL,
                      endpoint = NULL, api_version = NULL, profile = NULL,
                      region = NULL, project_id = NULL, location = NULL,
                      credentials = NULL, api_key = NULL, cache = NULL,
                      workspace = NULL, account = NULL, ...) {
  stopifnot(is.character(model), length(model) == 1L, nzchar(model))
  .contract_log$constructors[[length(.contract_log$constructors) + 1L]] <- list(
    provider = provider, model = model, base_url = base_url,
    endpoint = endpoint, api_version = api_version, profile = profile,
    region = region, project_id = project_id, location = location,
    workspace = workspace, account = account,
    has_credentials = !is.null(credentials), has_api_key = !is.null(api_key),
    cache = cache, params = params
  )
  registered <- list()
  turns <- list()
  last <- AssistantTurn(
    contents = list(ContentText(text = "")), finish_reason = "success"
  )

  record_call <- function(method, prompt, type = NULL) {
    .contract_log$calls[[length(.contract_log$calls) + 1L]] <- list(
      method = method,
      prompt = prompt,
      roles = vapply(turns, function(turn) turn_summary(turn)$role, character(1)),
      texts = vapply(turns, function(turn) turn_summary(turn)$text, character(1)),
      request_ids = vapply(
        turns, function(turn) turn_summary(turn)$request_id, character(1)
      ),
      type_class = if (is.null(type)) NULL else sub("^.*::", "", class(type)[1])
    )
  }

  list(
    set_turns = function(value) {
      stopifnot(
        is.list(value),
        all(vapply(value, function(turn) {
          S7::S7_inherits(turn, SystemTurn) ||
            S7::S7_inherits(turn, UserTurn) ||
            S7::S7_inherits(turn, AssistantTurn)
        }, logical(1)))
      )
      turns <<- value
      invisible(NULL)
    },
    register_tool = function(value) {
      stopifnot(inherits(value, "ellmer_tool"))
      registered[[length(registered) + 1L]] <<- value
      .contract_log$tools[[length(.contract_log$tools) + 1L]] <- list(
        name = value$name, required = value$required
      )
      invisible(NULL)
    },
    chat = function(prompt) {
      stopifnot(is.character(prompt), length(prompt) == 1L)
      record_call("chat", prompt)
      turns <<- c(turns, list(UserTurn(
        contents = list(ContentText(text = prompt))
      )))
      if (model %in% c("s7-model", "s7-tool-limit") && length(registered)) {
        args <- if (identical(model, "s7-model")) {
          list(name = "round")
        } else {
          list(x = 1)
        }
        request <- ContentToolRequest(
          id = "auto_1", name = registered[[1]]$name, arguments = args
        )
        turns <<- c(turns, list(AssistantTurn(
          contents = list(request), finish_reason = "tool"
        )))
        result <- do.call(registered[[1]]$fun, args)
        turns <<- c(turns, list(UserTurn(
          contents = list(ContentToolResult(request = request, value = result))
        )))
        if (identical(model, "s7-tool-limit")) {
          do.call(registered[[1]]$fun, args)
        }
      }
      last <<- AssistantTurn(
        contents = list(ContentText(text = "gathered context")),
        finish_reason = "success"
      )
      turns <<- c(turns, list(last))
      "gathered context"
    },
    chat_structured = function(prompt, type) {
      stopifnot(
        is.character(prompt), length(prompt) == 1L,
        S7::S7_inherits(type, TypeJsonSchema)
      )
      record_call("chat_structured", prompt, type)
      if (identical(model, "s7-secret-error")) {
        stop("provider rejected api_key ", api_key)
      }
      last <<- AssistantTurn(
        contents = list(ContentText(text = "structured")),
        finish_reason = "success"
      )
      list(
        r_code = "x <- 1", assumptions = list("none"), confidence = 0.9
      )
    },
    get_tokens = function() list(input = 100, output = 25),
    get_cost = function(include = "last") {
      stopifnot(identical(include, "last"))
      0.005
    },
    get_turns = function(include_system_prompt = FALSE) {
      if (isTRUE(include_system_prompt)) return(turns)
      Filter(function(turn) !S7::S7_inherits(turn, SystemTurn), turns)
    },
    last_turn = function() last
  )
}

chat_aws_bedrock <- function(system_prompt = NULL, base_url = NULL, model = NULL,
                             profile = NULL,
                             cache = c("auto", "5m", "1h", "none"),
                             params = NULL, api_args = list(),
                             api_headers = character(), echo = NULL) {
  cache <- match.arg(cache)
  make_chat("bedrock", model, params, base_url = base_url, profile = profile,
            cache = cache)
}

chat_azure_openai <- function(endpoint = NULL, model, params = NULL,
                              api_version = NULL, system_prompt = NULL,
                              api_key = NULL, credentials = NULL,
                              echo = NULL) {
  make_chat("azure", model, params, endpoint = endpoint,
            api_version = api_version, credentials = credentials,
            api_key = api_key)
}

chat_google_vertex <- function(location = NULL, project_id = NULL,
                               system_prompt = NULL, model = NULL,
                               params = NULL, api_args = list(),
                               api_headers = character(), echo = NULL) {
  make_chat("vertex", model, params, project_id = project_id,
            location = location)
}

models_aws_bedrock <- function(profile = NULL, base_url = NULL) {
  .contract_log$inventories[[length(.contract_log$inventories) + 1L]] <- list(
    provider = "bedrock", profile = profile, base_url = base_url
  )
  data.frame(id = "s7-bedrock")
}

models_google_vertex <- function(location = NULL, project_id = NULL,
                                 credentials = NULL) {
  .contract_log$inventories[[length(.contract_log$inventories) + 1L]] <- list(
    provider = "vertex", project_id = project_id, location = location,
    has_credentials = !is.null(credentials)
  )
  list(list(model_id = "s7-vertex"))
}

chat_ollama <- function(base_url = NULL, model, params = NULL, ...) {
  make_chat("ollama", model, params, base_url = base_url, ...)
}

chat_openai <- function(base_url = NULL, model, params = NULL,
                        credentials = NULL, api_key = NULL, ...) {
  make_chat("openai", model, params, base_url = base_url,
            credentials = credentials, api_key = api_key, ...)
}

# Mirrors of ellmer's own `databricks_workspace()` and `snowflake_account()`
# defaults: both constructors resolve their tenant ambiently when the caller
# omits the argument, so the fixture mirrors that lazy default rather than
# encoding a required argument ellmer does not have. Only the documented
# environment variable is read -- never a developer credential file or CLI
# profile.

fixture_databricks_workspace <- function() {
  host <- Sys.getenv("DATABRICKS_HOST", unset = "")
  if (!nzchar(host)) stop("No env var DATABRICKS_HOST set")
  if (!grepl("^https?://", host)) host <- paste0("https://", host)
  host
}

fixture_snowflake_account <- function() {
  account <- Sys.getenv("SNOWFLAKE_ACCOUNT", unset = "")
  if (!nzchar(account)) stop("Can't find env var SNOWFLAKE_ACCOUNT")
  account
}

# The seven additional agreed providers. Each formal list mirrors the public
# ellmer 0.4.2 signature -- names, order, and defaults -- so a sas2r argument
# that ellmer does not publish is a hard fixture error rather than a silent
# pass, and an omitted argument exercises ellmer's real public default.

chat_anthropic <- function(system_prompt = NULL, params = NULL, model = NULL,
                           cache = c("5m", "1h", "none"), api_args = list(),
                           base_url = "https://api.anthropic.com/v1",
                           beta_headers = character(), api_key = NULL,
                           credentials = NULL, api_headers = character(),
                           echo = NULL) {
  cache <- match.arg(cache)
  make_chat("anthropic", model, params, base_url = base_url,
            credentials = credentials, api_key = api_key, cache = cache)
}

chat_databricks <- function(workspace = fixture_databricks_workspace(),
                            system_prompt = NULL, model = NULL, token = NULL,
                            params = NULL, api_args = list(),
                            echo = c("none", "output", "all"),
                            api_headers = character()) {
  stopifnot(is.character(workspace), length(workspace) == 1L, nzchar(workspace))
  make_chat("databricks", model, params, workspace = workspace)
}

chat_deepseek <- function(system_prompt = NULL,
                          base_url = "https://api.deepseek.com",
                          api_key = NULL, credentials = NULL, model = NULL,
                          params = NULL, api_args = list(), echo = NULL,
                          api_headers = character()) {
  make_chat("deepseek", model, params, base_url = base_url,
            credentials = credentials, api_key = api_key)
}

chat_github <- function(system_prompt = NULL,
                        base_url = "https://models.github.ai/inference/",
                        api_key = NULL, credentials = NULL, model = NULL,
                        params = NULL, api_args = list(), echo = NULL,
                        api_headers = character()) {
  make_chat("github", model, params, base_url = base_url,
            credentials = credentials, api_key = api_key)
}

chat_google_gemini <- function(
    system_prompt = NULL,
    base_url = "https://generativelanguage.googleapis.com/v1beta/",
    api_key = NULL, credentials = NULL, model = NULL, params = NULL,
    api_args = list(), api_headers = character(), echo = NULL) {
  make_chat("gemini", model, params, base_url = base_url,
            credentials = credentials, api_key = api_key)
}

chat_posit <- function(system_prompt = NULL,
                       base_url = "https://gateway.posit.ai",
                       credentials = NULL, model = NULL, params = NULL,
                       cache = c("5m", "1h", "none"), api_args = list(),
                       api_headers = character(), echo = NULL) {
  cache <- match.arg(cache)
  make_chat("posit", model, params, base_url = base_url,
            credentials = credentials, cache = cache)
}

chat_snowflake <- function(system_prompt = NULL,
                           account = fixture_snowflake_account(),
                           credentials = NULL, model = NULL, params = NULL,
                           api_args = list(),
                           echo = c("none", "output", "all"),
                           api_headers = character()) {
  stopifnot(is.character(account), length(account) == 1L, nzchar(account))
  make_chat("snowflake", model, params, account = account,
            credentials = credentials)
}

record_inventory <- function(provider, ...) {
  .contract_log$inventories[[length(.contract_log$inventories) + 1L]] <- c(
    list(provider = provider), list(...)
  )
  invisible(NULL)
}

models_anthropic <- function(base_url = "https://api.anthropic.com/v1",
                             api_key = NULL, credentials = NULL) {
  record_inventory("anthropic", base_url = base_url,
                   has_credentials = !is.null(credentials),
                   has_api_key = !is.null(api_key))
  data.frame(id = "s7-anthropic")
}

models_deepseek <- function(base_url = "https://api.deepseek.com",
                            api_key = NULL, credentials = NULL) {
  record_inventory("deepseek", base_url = base_url,
                   has_credentials = !is.null(credentials),
                   has_api_key = !is.null(api_key))
  data.frame(id = "s7-deepseek")
}

models_github <- function(base_url = "https://models.github.ai/",
                          api_key = NULL, credentials = NULL) {
  record_inventory("github", base_url = base_url,
                   has_credentials = !is.null(credentials),
                   has_api_key = !is.null(api_key))
  data.frame(id = "s7-github")
}

models_google_gemini <- function(
    base_url = "https://generativelanguage.googleapis.com/v1beta/",
    api_key = NULL, credentials = NULL) {
  record_inventory("gemini", base_url = base_url,
                   has_credentials = !is.null(credentials),
                   has_api_key = !is.null(api_key))
  list(list(model_id = "s7-gemini"))
}

models_posit <- function(base_url = "https://gateway.posit.ai",
                         credentials = NULL) {
  record_inventory("posit", base_url = base_url,
                   has_credentials = !is.null(credentials))
  data.frame(id = "s7-posit")
}

models_openai <- function(base_url = "https://api.openai.com/v1",
                          api_key = NULL, credentials = NULL) {
  record_inventory("openai", base_url = base_url,
                   has_credentials = !is.null(credentials),
                   has_api_key = !is.null(api_key))
  data.frame(id = "s7-openai")
}

models_ollama <- function(base_url = "http://localhost:11434") {
  record_inventory("ollama", base_url = base_url)
  data.frame(id = "s7-ollama")
}
