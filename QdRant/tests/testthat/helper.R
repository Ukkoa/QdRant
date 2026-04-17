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
