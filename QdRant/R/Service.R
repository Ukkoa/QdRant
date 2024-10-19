
#' Services
#'
#' @description
#' This class provides methods to interact with the Qdrant service, including collecting telemetry data, retrieving instance details, gathering Prometheus metrics, and performing Kubernetes health checks.
#'
#' @field client A `QdrantClient` object used for making API requests.
#'
#' @export
Services <-
  R6::R6Class("Services",
              public = list(
                client = NULL,

                #' @description
                #' Initialize the Services object with a Qdrant client.
                #'
                #' @param client An instance of the `QdrantClient` class to interact with the Qdrant API.
                initialize = function(client) {
                  self$client <- client
                  },

                #' @description
                #' Collect telemetry data from the Qdrant instance.
                #'
                #' @return A list containing telemetry data from Qdrant.
                #' @examples
                #' \dontrun{
                #'   services <- Services$new(client)
                #'   telemetry <- services$collect_telemetry_data()
                #' }
                collect_telemetry_data = function(){

                  url <- paste0(self$client$get_base_url(), "/telemetry")
                  return(self$client$make_request("GET", url))

                },

                #' @description
                #' Retrieve instance details of the Qdrant service.
                #'
                #' @return A list containing details of the Qdrant instance.
                #' @examples
                #' \dontrun{
                #'   details <- services$retrieve_instance_details()
                #' }
                retrieve_instance_details = function(){

                  url <- paste0(self$client$get_base_url())
                  return(self$client$make_request("GET", url))

                },

                #' @description
                #' Collect Prometheus metrics from the Qdrant instance.
                #'
                #' @param anonymize A logical value indicating whether to anonymize the metrics (default is TRUE).
                #'
                #' @return A list containing Prometheus metrics.
                #' @examples
                #' \dontrun{
                #'   metrics <- services$collect_prometheus_metrics(anonymize = TRUE)
                #' }
                collect_prometheus_metrics = function(anonymize = T){

                  url  <- paste0(self$client$get_base_url(), "/metrics")
                  body <- list(anonymize = anonymize)
                  return(self$client$make_request("GET", url))

                },

                #' @description
                #' Check if write protection is enabled for the Qdrant instance.
                #'
                #' @return A list indicating the current write protection status.
                #' @examples
                #' \dontrun{
                #'   write_protection <- services$check_write_protection()
                #' }
                check_write_protection = function(){

                  url  <- paste0(self$client$get_base_url(), "/locks")
                  return(self$client$make_request("GET", url))

                },

                #' @description
                #' Enable or disable write protection for the Qdrant instance.
                #'
                #' @param write A logical value indicating whether to enable (TRUE) or disable (FALSE) write protection (default is TRUE).
                #'
                #' @return The response from the API indicating the result of the write protection change.
                #' @examples
                #' \dontrun{
                #'   services$set_write_protection(write = TRUE)
                #' }
                set_write_protection = function(write = T){
                  ## this needs to be a query not ajson

                  url  <- paste0(self$client$get_base_url(), "/locks")
                  body <- list(write = write)
                  return(self$client$make_request("POST", url, body))

                },

                #' @description
                #' Perform a Kubernetes health check for the Qdrant instance.
                #'
                #' @return The response from the API indicating the health status of the Qdrant instance.
                #' @examples
                #' \dontrun{
                #'   health_status <- services$kubernetes_health_check()
                #' }
                kubernetes_health_check = function(){

                  url  <- paste0(self$client$get_base_url(), "/healthz")
                  return(self$client$make_request("GET", url))

                },

                #' @description
                #' Perform a Kubernetes liveness probe for the Qdrant instance.
                #'
                #' @return The response from the API indicating the liveness status of the Qdrant instance.
                #' @examples
                #' \dontrun{
                #'   liveness_status <- services$kubernetes_liveness_probe()
                #' }
                kubernetes_liveness_probe = function(){

                  url  <- paste0(self$client$get_base_url(), "/livez")
                  return(self$client$make_request("GET", url))

                },

                #' @description
                #' Perform a Kubernetes readiness probe for the Qdrant instance.
                #'
                #' @return The response from the API indicating the readiness status of the Qdrant instance.
                #' @examples
                #' \dontrun{
                #'   readiness_status <- services$kubernetes_readiness_probe()
                #' }
                kubernetes_readiness_probe = function(){

                  url  <- paste0(self$client$get_base_url(), "/readyz")
                  return(self$client$make_request("GET", url))

                }

                )
  )
