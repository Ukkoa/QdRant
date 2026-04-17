
#' Snapshots
#'
#' @description
#' Create, list, download, recover, and delete snapshots for Qdrant
#' collections and for the full storage.
#'
#' @field client A \code{QdrantClient} instance.
#'
#' @export
Snapshots <- R6::R6Class("Snapshots",
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

    # -------------------------------------------------------------------------
    # Collection snapshots
    # -------------------------------------------------------------------------

    #' @description
    #' List all snapshots for a collection.
    #'
    #' @param collection_name Name of the collection.
    #'
    #' @return API response list of snapshot descriptions.
    #'
    #' @examples
    #' \dontrun{
    #'   snapshots$list_collection_snapshots("my_collection")
    #' }
    list_collection_snapshots = function(collection_name) {
      stopifnot(is.character(collection_name), nzchar(collection_name))
      url <- paste0(self$client$get_base_url(),
                    "/collections/", collection_name, "/snapshots")
      self$client$make_request("GET", url)
    },

    #' @description
    #' Create a snapshot of a collection.
    #'
    #' @param collection_name Name of the collection.
    #'
    #' @return API response list with snapshot metadata.
    #'
    #' @examples
    #' \dontrun{
    #'   snapshots$create_collection_snapshot("my_collection")
    #' }
    create_collection_snapshot = function(collection_name) {
      stopifnot(is.character(collection_name), nzchar(collection_name))
      url <- paste0(self$client$get_base_url(),
                    "/collections/", collection_name, "/snapshots")
      self$client$make_request("POST", url)
    },

    #' @description
    #' Restore a collection from a snapshot.
    #'
    #' @param collection_name Name of the collection.
    #' @param location URL or file path to the snapshot.
    #' @param priority (Optional) Character. Restore priority: \code{"snapshot"},
    #'   \code{"replica"}, or \code{"no_sync"}.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   snapshots$recover_collection_from_snapshot(
    #'     "my_collection",
    #'     location = "https://example.com/my_snapshot.snapshot")
    #' }
    recover_collection_from_snapshot = function(collection_name, location,
                                                priority = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.character(location), nzchar(location))
      url  <- paste0(self$client$get_base_url(),
                     "/collections/", collection_name, "/snapshots/recover")
      body <- list(location = location)
      if (!is.null(priority)) body$priority <- priority
      self$client$make_request("PUT", url, body)
    },

    #' @description
    #' Delete a collection snapshot.
    #'
    #' @param collection_name Name of the collection.
    #' @param snapshot_name Name of the snapshot file.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   snapshots$delete_collection_snapshot("my_collection", "snap.snapshot")
    #' }
    delete_collection_snapshot = function(collection_name, snapshot_name) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.character(snapshot_name), nzchar(snapshot_name))
      url <- paste0(self$client$get_base_url(),
                    "/collections/", collection_name,
                    "/snapshots/", snapshot_name)
      self$client$make_request("DELETE", url)
    },

    # -------------------------------------------------------------------------
    # Full-storage snapshots
    # -------------------------------------------------------------------------

    #' @description
    #' List all full-storage snapshots.
    #'
    #' @return API response list of snapshot descriptions.
    #'
    #' @examples
    #' \dontrun{
    #'   snapshots$list_full_snapshots()
    #' }
    list_full_snapshots = function() {
      url <- paste0(self$client$get_base_url(), "/snapshots")
      self$client$make_request("GET", url)
    },

    #' @description
    #' Create a full-storage snapshot (all collections).
    #'
    #' @return API response list with snapshot metadata.
    #'
    #' @examples
    #' \dontrun{
    #'   snapshots$create_full_snapshot()
    #' }
    create_full_snapshot = function() {
      url <- paste0(self$client$get_base_url(), "/snapshots")
      self$client$make_request("POST", url)
    },

    #' @description
    #' Delete a full-storage snapshot.
    #'
    #' @param snapshot_name Name of the snapshot file.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   snapshots$delete_full_snapshot("full_snap.snapshot")
    #' }
    delete_full_snapshot = function(snapshot_name) {
      stopifnot(is.character(snapshot_name), nzchar(snapshot_name))
      url <- paste0(self$client$get_base_url(), "/snapshots/", snapshot_name)
      self$client$make_request("DELETE", url)
    }
  )
)
