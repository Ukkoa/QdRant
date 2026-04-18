# ---------------------------------------------------------------------------
# MockQdrantClient
#
# A lightweight drop-in for QdrantClient that records every make_request call
# and returns pre-queued responses instead of hitting a real server.
#
# Usage:
#   mock <- MockQdrantClient$new()
#   mock$enqueue(list(status = "ok", result = TRUE))
#   cols  <- Collections$new(mock)
#   cols$list_all_collections()
#   mock$last_call   # inspect method/url/body/query
# ---------------------------------------------------------------------------

MockQdrantClient <- R6::R6Class("MockQdrantClient",
  cloneable = FALSE,
  public = list(
    host      = "localhost",
    port      = 6333,
    api_key   = NULL,
    last_call = NULL,
    calls     = NULL,
    .responses = NULL,

    initialize = function() {
      self$calls      <- list()
      self$.responses <- list()
    },

    enqueue = function(response) {
      self$.responses <- c(self$.responses, list(response))
      invisible(self)
    },

    get_base_url = function() {
      "http://localhost:6333"
    },

    make_request = function(method, url, body = NULL, query = NULL) {
      call <- list(method = method, url = url, body = body, query = query)
      self$last_call <- call
      self$calls     <- c(self$calls, list(call))

      if (length(self$.responses) > 0) {
        resp <- self$.responses[[1]]
        self$.responses <- self$.responses[-1]
        return(resp)
      }
      list(status = "ok", result = TRUE)
    }
  )
)

# ---------------------------------------------------------------------------
# Docker: spin up qdrant/qdrant automatically when no host is configured
# ---------------------------------------------------------------------------

local({
  # Only auto-start if no host is already configured
  if (nzchar(Sys.getenv("QDRANT_HOST"))) return(invisible(NULL))

  # Check Docker is available
  if (system("docker info", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    return(invisible(NULL))
  }

  # Check if Qdrant is already listening on 6333
  already_up <- tryCatch(
    { httr::GET("http://localhost:6333", httr::timeout(1)); TRUE },
    error = function(e) FALSE
  )
  if (already_up) {
    Sys.setenv(QDRANT_HOST = "localhost", QDRANT_PORT = "6333")
    return(invisible(NULL))
  }

  # Start the container
  container_id <- trimws(system(
    "docker run -d --rm -p 6333:6333 qdrant/qdrant",
    intern = TRUE, ignore.stderr = TRUE
  ))
  if (!nzchar(container_id)) return(invisible(NULL))

  message("Started Qdrant container ", substr(container_id, 1, 12),
          " — will stop when tests finish.")
  Sys.setenv(QDRANT_HOST = "localhost", QDRANT_PORT = "6333")

  # Poll until ready (up to 30 s)
  ready <- FALSE
  for (i in seq_len(60)) {
    Sys.sleep(0.5)
    ready <- tryCatch(
      { httr::GET("http://localhost:6333", httr::timeout(1)); TRUE },
      error = function(e) FALSE
    )
    if (ready) break
  }
  if (!ready) {
    warning("Qdrant container started but did not become ready — integration tests will be skipped.")
    system(paste("docker stop", container_id),
           ignore.stdout = TRUE, ignore.stderr = TRUE)
    return(invisible(NULL))
  }

  # Tear down when the test session ends
  withr::defer(
    {
      message("Stopping Qdrant container ", substr(container_id, 1, 12), "...")
      system(paste("docker stop", container_id),
             ignore.stdout = TRUE, ignore.stderr = TRUE)
      Sys.unsetenv("QDRANT_HOST")
    },
    envir = testthat::teardown_env()
  )
})

# Construct a sub-class from a MockQdrantClient.
mock_new <- function(Class, mock) {
  Class$new(
    req_fn      = function(...) mock$make_request(...),
    base_url_fn = function()    mock$get_base_url()
  )
}

# ---------------------------------------------------------------------------
# Integration-test helpers
# ---------------------------------------------------------------------------

# Returns a live QdrantClient when QDRANT_HOST is set, otherwise NULL.
live_client <- function() {
  host    <- Sys.getenv("QDRANT_HOST",    "")
  api_key <- Sys.getenv("QDRANT_API_KEY", "")
  port    <- as.integer(Sys.getenv("QDRANT_PORT", "6333"))
  if (!nzchar(host)) return(NULL)
  QdrantClient$new(
    host    = host,
    port    = if (is.na(port)) 6333L else port,
    api_key = if (nzchar(api_key)) api_key else NULL
  )
}

# Skip the current test when no live Qdrant instance is configured.
skip_if_no_qdrant <- function() {
  if (is.null(live_client())) {
    testthat::skip("No Qdrant instance available. Set QDRANT_HOST (and optionally QDRANT_API_KEY, QDRANT_PORT).")
  }
}

# A collection name that is unlikely to collide with real data.
test_collection <- function(suffix = "") {
  paste0("qdrant_r_test_", format(Sys.time(), "%Y%m%d%H%M%S"), suffix)
}
