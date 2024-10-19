

#' QdrantClient
#'
#' @description
#' This class provides methods to interact with a Qdrant instance.
#' It handles API requests to manage vectors, collections, and other resources.
#'
#' @field host The host URL for the Qdrant instance (default: "localhost").
#' @field port The port on which Qdrant is running (default: 6333).
#' @field api_key The API key for authentication (optional).
#'
#' @export
QdrantClient <- R6::R6Class("QdrantClient",
                        public = list(
                          host = NULL,
                          port = NULL,
                          api_key = NULL,

                          #' @description
                          #' Initialize the Qdrant client.
                          #'
                          #' @param host The host URL for the Qdrant instance.
                          #' @param port The port number on which Qdrant is running.
                          #' @param api_key API key for authentication (optional).
                          initialize = function(host = "localhost", port = 6333, api_key = NULL) {
                            self$host    <- host
                            self$port    <- port
                            self$api_key <- api_key
                          },

                          #' @description
                          #' Get the base URL of the Qdrant instance.
                          #'
                          #' @return A character string with the full base URL.
                          get_base_url = function() {

                            if(self$host == 'localhost'){
                              return(paste0("http://", self$host, ":", self$port))
                            } else {
                              return(paste0("https://", self$host, ":", self$port))
                            }
                          },


                          #' @description
                          #' Make an HTTP request to the Qdrant API.
                          #'
                          #' @param method HTTP method to use (GET, POST, PUT, DELETE).
                          #' @param url The full URL for the API request.
                          #' @param body (Optional) The request body for POST or PUT methods.
                          #'
                          #' @return Parsed response content from the API.
                          #' @examples
                          #' \dontrun{
                          #'   client <- QdrantClient$new()
                          #'   base_url <- client$get_base_url()
                          #'   response <- client$make_request("GET", paste0(base_url, "/collections"))
                          #' }
                          make_request = function(method, url, body = NULL) {
                            headers <- if (!is.null(self$api_key)) add_headers(`api-key` = self$api_key) else NULL

                            res <- switch(
                              method,
                              "PUT"    = PUT(url, body = body, encode = "json", headers),
                              "POST"   = POST(url, body = body, encode = "json", headers),
                              "GET"    = GET(url, headers),
                              "DELETE" = DELETE(url, headers),
                              stop("Unsupported HTTP method")
                            )
                            stop_for_status(res)
                            return(content(res))
                          }
                        )
                        )




