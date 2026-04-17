
#' Services
#'
#' @description
#' Health checks, telemetry, metrics, and write-lock management for a Qdrant
#' instance.
#'
#' @field client A \code{QdrantClient} instance.
#'
#' @export
Services <- R6::R6Class("Services",
  cloneable = FALSE,
  public = list(
    client = NULL,

    #' @description
    #' Initialize with a \code{QdrantClient}.
    #'
    #' @param client A \code{QdrantClient} instance.
    initialize = function(client) {
      self$client <- client
    },

    #' @description
    #' Get Qdrant instance info (version, commit).
    #'
    #' @return Named list with \code{version} and \code{commit}.
    #'
    #' @examples
    #' \dontrun{
    #'   services$retrieve_instance_details()
    #' }
    retrieve_instance_details = function() {
      url <- self$client$get_base_url()
      self$client$make_request("GET", url)
    },

    #' @description
    #' Collect detailed telemetry data about the Qdrant instance.
    #'
    #' @param anonymize Logical. Anonymize sensitive fields (default TRUE).
    #'
    #' @return Named list of telemetry metrics.
    #'
    #' @examples
    #' \dontrun{
    #'   services$collect_telemetry_data()
    #' }
    collect_telemetry_data = function(anonymize = TRUE) {
      url   <- paste0(self$client$get_base_url(), "/telemetry")
      query <- list(anonymize = tolower(as.character(anonymize)))
      self$client$make_request("GET", url, query = query)
    },

    #' @description
    #' Collect Prometheus-compatible metrics.
    #'
    #' @param anonymize Logical. Anonymize collection/field names (default TRUE).
    #'
    #' @return Raw Prometheus text (character string).
    #'
    #' @examples
    #' \dontrun{
    #'   services$collect_prometheus_metrics()
    #' }
    collect_prometheus_metrics = function(anonymize = TRUE) {
      url   <- paste0(self$client$get_base_url(), "/metrics")
      query <- list(anonymize = tolower(as.character(anonymize)))
      self$client$make_request("GET", url, query = query)
    },

    #' @description
    #' Check current write-lock status.
    #'
    #' @return Named list with a \code{write} boolean.
    #'
    #' @examples
    #' \dontrun{
    #'   services$check_write_protection()
    #' }
    check_write_protection = function() {
      url <- paste0(self$client$get_base_url(), "/locks")
      self$client$make_request("GET", url)
    },

    #' @description
    #' Enable or disable write protection on the Qdrant instance.
    #'
    #' @param write Logical. \code{TRUE} to enable write lock (default TRUE).
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   services$set_write_protection(write = FALSE)
    #' }
    set_write_protection = function(write = TRUE) {
      url  <- paste0(self$client$get_base_url(), "/locks")
      body <- list(write = write)
      self$client$make_request("POST", url, body)
    },

    #' @description
    #' Retrieve reported operational issues.
    #'
    #' @return Named list of active issues.
    #'
    #' @examples
    #' \dontrun{
    #'   services$get_issues()
    #' }
    get_issues = function() {
      url <- paste0(self$client$get_base_url(), "/issues")
      self$client$make_request("GET", url)
    },

    #' @description
    #' Clear all reported operational issues.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   services$clear_issues()
    #' }
    clear_issues = function() {
      url <- paste0(self$client$get_base_url(), "/issues")
      self$client$make_request("DELETE", url)
    },

    #' @description
    #' Kubernetes combined health check (\code{/healthz}).
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   services$kubernetes_health_check()
    #' }
    kubernetes_health_check = function() {
      url <- paste0(self$client$get_base_url(), "/healthz")
      self$client$make_request("GET", url)
    },

    #' @description
    #' Kubernetes liveness probe (\code{/livez}).
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   services$kubernetes_liveness_probe()
    #' }
    kubernetes_liveness_probe = function() {
      url <- paste0(self$client$get_base_url(), "/livez")
      self$client$make_request("GET", url)
    },

    #' @description
    #' Kubernetes readiness probe (\code{/readyz}).
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   services$kubernetes_readiness_probe()
    #' }
    kubernetes_readiness_probe = function() {
      url <- paste0(self$client$get_base_url(), "/readyz")
      self$client$make_request("GET", url)
    }
  )
)
