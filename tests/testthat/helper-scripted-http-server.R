# A loopback HTTP server that answers each request with the next scripted body
# and then closes the connection. It lets a test drive the real
# ellmer/httr2/curl request path, streamed or not, without a provider.
#
# `bodies` is one response body per expected request, in order. The server
# records each request body so a test can check what was put on the wire.
local_scripted_http_server <- function(bodies, content_type = "text/event-stream",
                                       env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  port_file <- file.path(dir, "port")
  log_file <- file.path(dir, "requests.log")
  process <- callr::r_bg(
    function(bodies, content_type, port_file, log_file) {
      server <- NULL
      for (port in sample(20000:45000, 50L)) {
        server <- tryCatch(serverSocket(port), error = function(error) NULL)
        if (!is.null(server)) break
      }
      if (is.null(server)) stop("no free loopback port")
      on.exit(close(server), add = TRUE)
      writeLines(as.character(port), port_file)
      for (body in bodies) {
        con <- socketAccept(server, blocking = TRUE, open = "a+b", timeout = 60)
        content_length <- 0L
        repeat {
          line <- readLines(con, n = 1L)
          if (!length(line) || !nzchar(line)) break
          if (grepl("^content-length:", line, ignore.case = TRUE)) {
            content_length <- as.integer(sub("^[^:]*:\\s*", "", line))
          }
        }
        payload <- raw()
        while (length(payload) < content_length) {
          chunk <- readBin(con, "raw", n = content_length - length(payload))
          if (!length(chunk)) break
          payload <- c(payload, chunk)
        }
        cat(rawToChar(payload), "\n", sep = "", file = log_file, append = TRUE)
        writeBin(charToRaw(paste0(
          "HTTP/1.1 200 OK\r\nContent-Type: ", content_type,
          "\r\nConnection: close\r\n\r\n", body
        )), con)
        close(con)
      }
    },
    args = list(bodies, content_type, port_file, log_file)
  )
  withr::defer(process$kill(), envir = env)
  deadline <- Sys.time() + 15
  while (!file.exists(port_file) || !length(readLines(port_file, warn = FALSE))) {
    if (Sys.time() > deadline || !process$is_alive()) {
      stop("scripted HTTP server did not start: ", process$read_error())
    }
    Sys.sleep(0.05)
  }
  list(
    url = paste0("http://127.0.0.1:", readLines(port_file, warn = FALSE)[[1]]),
    requests = function() {
      if (!file.exists(log_file)) return(list())
      lapply(readLines(log_file, warn = FALSE), jsonlite::parse_json)
    }
  )
}

# Anthropic's Messages API as a server-sent event stream. `blocks` is the
# assistant turn's content: `list(type = "text", text = ...)` or
# `list(type = "tool_use", id = ..., name = ..., input = list(...))`.
# `complete = FALSE` ends the stream before the provider's closing events, as
# a dropped connection does. `error` is a provider error (`type`, `message`)
# sent as an event in place of those closing events.
anthropic_event_stream <- function(blocks, stop_reason = "end_turn",
                                   output_tokens = 9L, complete = TRUE,
                                   error = NULL) {
  if (!is.null(error)) complete <- FALSE
  event <- function(data) paste0(
    "event: ", data$type, "\ndata: ",
    jsonlite::toJSON(data, auto_unbox = TRUE, null = "null"), "\n\n"
  )
  events <- event(list(type = "message_start", message = list(
    id = "msg_scripted", type = "message", role = "assistant",
    model = "claude-scripted", content = list(), stop_reason = NULL,
    usage = list(input_tokens = 11L, output_tokens = 1L)
  )))
  for (index in seq_along(blocks)) {
    block <- blocks[[index]]
    position <- index - 1L
    if (identical(block$type, "tool_use")) {
      start <- list(type = "tool_use", id = block$id, name = block$name,
                    input = structure(list(), names = character()))
      delta <- list(type = "input_json_delta", partial_json = as.character(
        jsonlite::toJSON(block$input, auto_unbox = TRUE)
      ))
    } else {
      start <- list(type = "text", text = "")
      delta <- list(type = "text_delta", text = block$text)
    }
    events <- c(
      events,
      event(list(type = "content_block_start", index = position, content_block = start)),
      event(list(type = "ping")),
      event(list(type = "content_block_delta", index = position, delta = delta)),
      if (complete) event(list(type = "content_block_stop", index = position))
    )
  }
  if (!is.null(error)) {
    events <- c(events, event(list(type = "error", error = error)))
  }
  if (complete) {
    events <- c(
      events,
      event(list(type = "message_delta", delta = list(stop_reason = stop_reason),
                 usage = list(output_tokens = output_tokens,
                              output_tokens_details = list(thinking_tokens = 4L)))),
      event(list(type = "message_stop"))
    )
  }
  paste0(events, collapse = "")
}

# The same assistant turn as one whole (non-streamed) Messages API response.
anthropic_message_body <- function(blocks, stop_reason = "end_turn",
                                   output_tokens = 9L) {
  as.character(jsonlite::toJSON(list(
    id = "msg_scripted", type = "message", role = "assistant",
    model = "claude-scripted", content = blocks, stop_reason = stop_reason,
    usage = list(input_tokens = 11L, output_tokens = output_tokens,
                 output_tokens_details = list(thinking_tokens = 4L))
  ), auto_unbox = TRUE))
}
