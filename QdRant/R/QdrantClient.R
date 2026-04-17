
#' QdrantClient
#'
#' @description
#' HTTP client for the Qdrant vector database REST API. All higher-level classes
#' (Collections, Points, Search, etc.) hold a reference to an instance of this class.
#'
#' @field host Hostname or IP of the Qdrant instance (default: \code{"localhost"}).
#' @field port Port number (default: \code{6333}).
#' @field api_key API key for authentication (optional, for Qdrant Cloud).
#'
#' @export
QdrantClient <- R6::R6Class("QdrantClient",
  cloneable = FALSE,
  public = list(
    host    = NULL,
    port    = NULL,
    api_key = NULL,

    #' @description
    #' Create a new QdrantClient.
    #'
    #' @param host Hostname or IP of the Qdrant instance.
    #' @param port Port number.
    #' @param api_key API key for Qdrant Cloud (optional).
    #'
    #' @examples
    #' \dontrun{
    #'   # Local instance
    #'   client <- QdrantClient$new()
    #'
    #'   # Qdrant Cloud
    #'   client <- QdrantClient$new(
    #'     host    = "xyz.us-east-1-0.aws.cloud.qdrant.io",
    #'     port    = 6333,
    #'     api_key = Sys.getenv("QDRANT_API_KEY")
    #'   )
    #' }
    initialize = function(host = "localhost", port = 6333, api_key = NULL) {
      self$host    <- host
      self$port    <- port
      self$api_key <- api_key
    },

    #' @description
    #' Build the base URL for the Qdrant instance.
    #'
    #' Uses \code{http://} for localhost/loopback and \code{https://} for all
    #' other hosts.
    #'
    #' @return A character string such as \code{"https://host:6333"}.
    get_base_url = function() {
      scheme <- if (self$host %in% c("localhost", "127.0.0.1", "::1")) "http" else "https"
      paste0(scheme, "://", self$host, ":", self$port)
    },

    #' @description
    #' Make an HTTP request to the Qdrant API.
    #'
    #' @param method HTTP method: \code{"GET"}, \code{"POST"}, \code{"PUT"},
    #'   \code{"PATCH"}, or \code{"DELETE"}.
    #' @param url Full URL for the request.
    #' @param body Named list to serialise as JSON body (optional).
    #' @param query Named list of URL query parameters (optional).
    #'
    #' @return Parsed response content (R list).
    make_request = function(method, url, body = NULL, query = NULL) {
      headers <- if (!is.null(self$api_key)) {
        httr::add_headers(`api-key` = self$api_key)
      } else {
        NULL
      }

      res <- switch(
        method,
        "GET"    = httr::GET(url,    query = query, headers),
        "POST"   = httr::POST(url,   query = query, body = body, encode = "json", headers),
        "PUT"    = httr::PUT(url,    query = query, body = body, encode = "json", headers),
        "PATCH"  = httr::PATCH(url,  query = query, body = body, encode = "json", headers),
        "DELETE" = httr::DELETE(url, query = query, body = body, encode = "json", headers),
        stop("Unsupported HTTP method: ", method)
      )

      httr::stop_for_status(res)
      httr::content(res)
    }
  )
)
