
#' QdrantClient
#'
#' @description
#' Main entry point for the Qdrant REST API. Instantiating this class gives you
#' access to all API namespaces as sub-objects:
#' \code{$collections}, \code{$points}, \code{$search}, \code{$indexes},
#' \code{$aliases}, \code{$snapshots}, and \code{$service}.
#'
#' @field collections A \code{Collections} instance.
#' @field points A \code{Points} instance.
#' @field search A \code{Search} instance.
#' @field indexes An \code{Indexes} instance.
#' @field aliases An \code{Aliases} instance.
#' @field snapshots A \code{Snapshots} instance.
#' @field service A \code{Services} instance.
#'
#' @export
QdrantClient <- R6::R6Class("QdrantClient",
  cloneable = FALSE,
  public = list(
    collections = NULL,
    points      = NULL,
    search      = NULL,
    indexes     = NULL,
    aliases     = NULL,
    snapshots   = NULL,
    service     = NULL,

    #' @description
    #' Create a new QdrantClient.
    #'
    #' @param host        Hostname or IP of the Qdrant instance.
    #' @param port        Port number.
    #' @param api_key     API key for Qdrant Cloud (optional).
    #' @param max_retries Integer. How many times to retry transient failures
    #'   (HTTP 429/502/503/504 or network errors) before giving up. Default 3.
    #'   Set to 1 to disable retries.
    #'
    #' @examples
    #' \dontrun{
    #'   # Local instance
    #'   client <- QdrantClient$new()
    #'
    #'   # Qdrant Cloud with custom retry count
    #'   client <- QdrantClient$new(
    #'     host        = "xyz.us-east-1-0.aws.cloud.qdrant.io",
    #'     port        = 6333,
    #'     api_key     = Sys.getenv("QDRANT_API_KEY"),
    #'     max_retries = 5L
    #'   )
    #' }
    initialize = function(host = "localhost", port = 6333, api_key = NULL,
                          max_retries = 3L) {
      stopifnot(is.numeric(max_retries), max_retries >= 1)
      private$host        <- host
      private$port        <- port
      private$api_key     <- api_key
      private$max_retries <- as.integer(max_retries)

      req      <- function(...) private$make_request(...)
      base_url <- function()    private$get_base_url()

      self$collections <- Collections$new(req, base_url)
      self$points      <- Points$new(req, base_url)
      self$search      <- Search$new(req, base_url)
      self$indexes     <- Indexes$new(req, base_url)
      self$aliases     <- Aliases$new(req, base_url)
      self$snapshots   <- Snapshots$new(req, base_url)
      self$service     <- Services$new(req, base_url)
    }
  ),
  private = list(
    host        = NULL,
    port        = NULL,
    api_key     = NULL,
    max_retries = NULL,

    get_base_url = function() {
      scheme <- if (private$host %in% c("localhost", "127.0.0.1", "::1")) "http" else "https"
      paste0(scheme, "://", private$host, ":", private$port)
    },

    make_request = function(method, url, body = NULL, query = NULL) {
      headers <- if (!is.null(private$api_key)) {
        httr::add_headers(`api-key` = private$api_key)
      } else {
        NULL
      }

      retryable <- c(429L, 502L, 503L, 504L)
      attempt   <- 0L

      repeat {
        attempt <- attempt + 1L

        res <- tryCatch(
          switch(
            method,
            "GET"    = httr::GET(url,    query = query, headers),
            "POST"   = httr::POST(url,   query = query, body = body, encode = "json", headers),
            "PUT"    = httr::PUT(url,    query = query, body = body, encode = "json", headers),
            "PATCH"  = httr::PATCH(url,  query = query, body = body, encode = "json", headers),
            "DELETE" = httr::DELETE(url, query = query, body = body, encode = "json", headers),
            stop("Unsupported HTTP method: ", method)
          ),
          error = function(e) e
        )

        if (inherits(res, "error")) {
          if (attempt >= private$max_retries) stop(res)
          Sys.sleep(2 ^ (attempt - 1))
          next
        }

        if (httr::status_code(res) %in% retryable && attempt < private$max_retries) {
          Sys.sleep(2 ^ (attempt - 1))
          next
        }

        httr::stop_for_status(res)
        return(httr::content(res))
      }
    }
  )
)
