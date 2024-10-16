
# everything is get except one.
#' ServiceManager Class
#'
#' This R6 class provides methods to interact with the Qdrant API, including
#' telemetry collection, Prometheus metrics, Kubernetes health checks,
#' and write protection settings.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data telemetry prometheus kubernetes locks
#' @return Object of \code{\link{R6Class}} with methods to interact with the Qdrant API.
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(client)}}{Constructor method. Initializes the `ServiceManager` class with a Qdrant client.}
#'   \item{\code{collect_telemetry_data()}}{Collects telemetry data from the Qdrant instance.
#'   Sends a GET request to `/telemetry/` endpoint.}
#'   \item{\code{retrieve_instance_details()}}{Retrieves instance details from the Qdrant base URL.
#'   Sends a GET request to the base URL.}
#'   \item{\code{collect_prometheus_metrics(anonymize = TRUE)}}{Collects Prometheus metrics from the Qdrant instance.
#'   Sends a GET request to `/metrics/` with an optional `anonymize` query parameter (default: TRUE).}
#'   \item{\code{check_write_protection()}}{Checks the current write protection status of the Qdrant instance.
#'   Sends a GET request to `/locks/`.}
#'   \item{\code{set_write_protection(write = TRUE)}}{Sets the write protection status of the Qdrant instance.
#'   Sends a POST request to `/locks/` with a body parameter `write`.}
#'   \item{\code{kubernetes_health_check()}}{Performs a Kubernetes health check on the Qdrant instance.
#'   Sends a GET request to `/healthz/`.}
#'   \item{\code{kubernetes_liveness_probe()}}{Performs a Kubernetes liveness probe on the Qdrant instance.
#'   Sends a GET request to `/livez/`.}
#'   \item{\code{kubernetes_readiness_probe()}}{Performs a Kubernetes readiness probe on the Qdrant instance.
#'   Sends a GET request to `/readyz/`.}
#' }
#'
#' @examples
#' # Assuming `client` is an initialized Qdrant client object
#' service_manager <- ServiceManager$new(client)
#' service_manager$collect_telemetry_data()
#' service_manager$retrieve_instance_details()
#' service_manager$collect_prometheus_metrics(anonymize = TRUE)
#' service_manager$check_write_protection()
#' service_manager$set_write_protection(write = FALSE)
#' service_manager$kubernetes_health_check()
#' service_manager$kubernetes_liveness_probe()
#' service_manager$kubernetes_readiness_probe()
#' @export
ServiceManager <-
  R6::R6Class("ServiceManager",
              public = list(
                client = NULL,
                initialize = function(client) {
                  self$client <- client
                  },
                collect_telemetry_data = function(){

                  url <- paste0(self$client$get_base_url(), "/telemetry")
                  return(self$client$make_request("GET", url))

                },
                retrieve_instance_details = function(){

                  url <- paste0(self$client$get_base_url())
                  return(self$client$make_request("GET", url))

                },
                collect_prometheus_metrics = function(anonymize = T){

                  url  <- paste0(self$client$get_base_url(), "/metrics")
                  body <- list(anonymize = anonymize)
                  return(self$client$make_request("GET", url))

                },
                check_write_protection = function(){

                  url  <- paste0(self$client$get_base_url(), "/locks")
                  return(self$client$make_request("GET", url))

                },
                set_write_protection = function(write = T){
                  ## this needs to be a query not ajson

                  url  <- paste0(self$client$get_base_url(), "/locks")
                  body <- list(write = write)
                  return(self$client$make_request("POST", url))

                },
                kubernetes_health_check = function(){

                  url  <- paste0(self$client$get_base_url(), "/healthz")
                  return(self$client$make_request("GET", url))

                },
                kubernetes_liveness_probe = function(){

                  url  <- paste0(self$client$get_base_url(), "/livez")
                  return(self$client$make_request("GET", url))

                },
                kubernetes_readiness_probe = function(){

                  url  <- paste0(self$client$get_base_url(), "/readyz")
                  return(self$client$make_request("GET", url))

                }

                )
  )
