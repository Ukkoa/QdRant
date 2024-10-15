

#' QdrantClient Class
#'
#' A client for interacting with the Qdrant API. This class provides methods
#' to send HTTP requests (GET, POST, PUT, DELETE) to the Qdrant server and
#' handle API authentication and URL construction.
#'
#' @docType class
#' @format An R6 class object.
#' @import httr
#' @field host Character. The hostname or IP address of the Qdrant server. Default is "localhost".
#' @field port Numeric. The port number of the Qdrant server. Default is 6333.
#' @field api_key Character. Optional API key for authenticating requests. Default is NULL.
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(host = "localhost", port = 6333, api_key = NULL)}}{Initializes a new QdrantClient object with a specified host, port, and optional API key.}
#'   \item{\code{get_base_url()}}{Returns the base URL of the Qdrant server, using either HTTP or HTTPS based on the host.}
#'   \item{\code{make_request(method, url, body = NULL)}}{Sends an HTTP request to the specified URL using the given method (GET, POST, PUT, DELETE). Optionally sends a body with the request.}
#' }
#' @examples
#' # Create a new Qdrant client instance
#' client <- QdrantClient$new(host = "localhost", port = 6333, api_key = "your-api-key")
#' @export
QdrantClient <- R6::R6Class("QdrantClient",
                        public = list(
                          host = NULL,
                          port = NULL,
                          api_key = NULL,

                          initialize = function(host = "localhost", port = 6333, api_key = NULL) {
                            self$host    <- host
                            self$port    <- port
                            self$api_key <- api_key
                          },

                          get_base_url = function() {

                            if(self$host == 'localhost'){
                              return(paste0("http://", self$host, ":", self$port))
                            } else {
                              return(paste0("https://", self$host, ":", self$port))
                            }
                          },

                          make_request = function(method, url, body = NULL) {
                            headers <- if (!is.null(self$api_key)) add_headers(`api-key` = self$api_key) else NULL
                            if (method == "PUT") {
                              res <- PUT(url, body = body, encode = "json", headers)
                            } else if (method == "POST") {
                              res <- POST(url, body = body, encode = "json", headers)
                            } else if (method == "GET") {
                              res <- GET(url, headers)
                            } else if (method == "DELETE") {
                              res <- DELETE(url, headers)
                            }
                            stop_for_status(res)
                            return(content(res))
                          }
                        ))
