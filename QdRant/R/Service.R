
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
  private = list(.req = NULL, .base_url = NULL),
  public = list(

    #' @description Initialize with closures provided by \code{QdrantClient}.
    #' @param req_fn Function. Makes HTTP requests.
    #' @param base_url_fn Function. Returns the base URL.
    initialize = function(req_fn, base_url_fn) {
      private$.req      <- req_fn
      private$.base_url <- base_url_fn
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
      url <- private$.base_url()
      private$.req("GET", url)
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
      url   <- paste0(private$.base_url(), "/telemetry")
      query <- list(anonymize = tolower(as.character(anonymize)))
      private$.req("GET", url, query = query)
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
      url   <- paste0(private$.base_url(), "/metrics")
      query <- list(anonymize = tolower(as.character(anonymize)))
      private$.req("GET", url, query = query)
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
      url <- paste0(private$.base_url(), "/locks")
      private$.req("GET", url)
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
      url  <- paste0(private$.base_url(), "/locks")
      body <- list(write = write)
      private$.req("POST", url, body)
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
      url <- paste0(private$.base_url(), "/issues")
      private$.req("GET", url)
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
      url <- paste0(private$.base_url(), "/issues")
      private$.req("DELETE", url)
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
      url <- paste0(private$.base_url(), "/healthz")
      private$.req("GET", url)
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
      url <- paste0(private$.base_url(), "/livez")
      private$.req("GET", url)
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
      url <- paste0(private$.base_url(), "/readyz")
      private$.req("GET", url)
    }
  )
)
