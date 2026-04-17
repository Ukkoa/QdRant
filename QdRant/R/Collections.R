
#' Collections
#'
#' @description
#' Manage Qdrant collections: create, update, delete, list, and inspect.
#'
#' @field client A \code{QdrantClient} instance.
#'
#' @export
Collections <- R6::R6Class("Collections",
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
    #' Create a new collection.
    #'
    #' @param collection_name Name of the collection to create.
    #' @param vector_size Integer. Dimensionality of stored vectors.
    #' @param distance_metric Distance function: \code{"Cosine"}, \code{"Dot"}, or \code{"Euclid"}.
    #' @param shard_number (Optional) Integer. Number of shards.
    #' @param sharding_method (Optional) Character. \code{"Auto"}, \code{"Custom"}, or \code{"Indexed"}.
    #' @param replication_factor (Optional) Integer. Replicas per shard.
    #' @param write_consistency_factor (Optional) Integer. Replicas that must ack writes.
    #' @param on_disk_payload (Optional) Logical. Store payload on disk.
    #' @param hnsw_config (Optional) Named list. HNSW index parameters.
    #' @param wal_config (Optional) Named list. Write-Ahead Log parameters.
    #' @param optimizers_config (Optional) Named list. Optimizer parameters.
    #' @param init_from (Optional) Named list with \code{collection} key — copy from existing collection.
    #' @param quantization_config (Optional) Named list. Quantization parameters.
    #' @param sparse_vectors (Optional) Named list. Sparse vector configurations.
    #' @param timeout (Optional) Numeric. Seconds to wait for replication acknowledgement.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   client      <- QdrantClient$new()
    #'   collections <- Collections$new(client)
    #'   collections$create_collection("my_collection", vector_size = 128)
    #' }
    create_collection = function(
        collection_name,
        vector_size,
        distance_metric       = "Cosine",
        shard_number          = NULL,
        sharding_method       = NULL,
        replication_factor    = NULL,
        write_consistency_factor = NULL,
        on_disk_payload       = NULL,
        hnsw_config           = NULL,
        wal_config            = NULL,
        optimizers_config     = NULL,
        init_from             = NULL,
        quantization_config   = NULL,
        sparse_vectors        = NULL,
        timeout               = NULL) {

      stopifnot(
        is.character(collection_name), nzchar(collection_name),
        is.numeric(vector_size),
        is.character(distance_metric),
        distance_metric %in% c("Cosine", "Dot", "Euclid")
      )

      body <- list(vectors = list(size = vector_size, distance = distance_metric))

      opt_fields <- c(
        "shard_number", "sharding_method", "replication_factor",
        "write_consistency_factor", "on_disk_payload",
        "hnsw_config", "wal_config", "optimizers_config",
        "init_from", "quantization_config", "sparse_vectors"
      )
      for (field in opt_fields) {
        val <- get(field, envir = environment())
        if (!is.null(val)) body[[field]] <- val
      }

      url   <- paste0(self$client$get_base_url(), "/collections/", collection_name)
      query <- if (!is.null(timeout)) list(timeout = timeout) else NULL
      self$client$make_request("PUT", url, body, query = query)
    },

    #' @description
    #' Update an existing collection's parameters.
    #'
    #' @param collection_name Name of the collection.
    #' @param vectors (Optional) Named list. Updated vector configurations.
    #' @param optimizers_config (Optional) Named list. Updated optimizer settings.
    #' @param params (Optional) Named list. Updated collection parameters.
    #' @param hnsw_config (Optional) Named list. Updated HNSW settings.
    #' @param quantization_config (Optional) Named list. Updated quantization settings.
    #' @param sparse_vectors (Optional) Named list. Updated sparse vector configurations.
    #' @param strict_mode_config (Optional) Named list. Strict mode settings.
    #' @param metadata (Optional) Named list. Arbitrary collection metadata.
    #' @param timeout (Optional) Numeric. Seconds to wait for acknowledgement.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   collections$update_collection("my_collection",
    #'     optimizers_config = list(indexing_threshold = 50000))
    #' }
    update_collection = function(
        collection_name,
        vectors             = NULL,
        optimizers_config   = NULL,
        params              = NULL,
        hnsw_config         = NULL,
        quantization_config = NULL,
        sparse_vectors      = NULL,
        strict_mode_config  = NULL,
        metadata            = NULL,
        timeout             = NULL) {

      stopifnot(is.character(collection_name), nzchar(collection_name))

      body <- list()
      opt_fields <- c(
        "vectors", "optimizers_config", "params", "hnsw_config",
        "quantization_config", "sparse_vectors", "strict_mode_config", "metadata"
      )
      for (field in opt_fields) {
        val <- get(field, envir = environment())
        if (!is.null(val)) body[[field]] <- val
      }

      url   <- paste0(self$client$get_base_url(), "/collections/", collection_name)
      query <- if (!is.null(timeout)) list(timeout = timeout) else NULL
      self$client$make_request("PATCH", url, body, query = query)
    },

    #' @description
    #' Get details of a specific collection.
    #'
    #' @param collection_name Name of the collection.
    #'
    #' @return API response list with collection info.
    #'
    #' @examples
    #' \dontrun{
    #'   collections$get_collection_details("my_collection")
    #' }
    get_collection_details = function(collection_name) {
      stopifnot(is.character(collection_name), nzchar(collection_name))
      url <- paste0(self$client$get_base_url(), "/collections/", collection_name)
      self$client$make_request("GET", url)
    },

    #' @description
    #' List all collections.
    #'
    #' @return API response list containing all collection names.
    #'
    #' @examples
    #' \dontrun{
    #'   collections$list_all_collections()
    #' }
    list_all_collections = function() {
      url <- paste0(self$client$get_base_url(), "/collections")
      self$client$make_request("GET", url)
    },

    #' @description
    #' Check whether a collection exists.
    #'
    #' @param collection_name Name of the collection.
    #'
    #' @return API response list with an \code{exists} boolean.
    #'
    #' @examples
    #' \dontrun{
    #'   collections$check_collection_existence("my_collection")
    #' }
    check_collection_existence = function(collection_name) {
      stopifnot(is.character(collection_name), nzchar(collection_name))
      url <- paste0(self$client$get_base_url(), "/collections/", collection_name, "/exists")
      self$client$make_request("GET", url)
    },

    #' @description
    #' Delete a collection.
    #'
    #' @param collection_name Name of the collection.
    #' @param timeout (Optional) Numeric. Seconds to wait for acknowledgement.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   collections$delete_collection("my_collection")
    #' }
    delete_collection = function(collection_name, timeout = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name))
      url   <- paste0(self$client$get_base_url(), "/collections/", collection_name)
      query <- if (!is.null(timeout)) list(timeout = timeout) else NULL
      self$client$make_request("DELETE", url, query = query)
    },

    #' @description
    #' Get the optimization status of a collection.
    #'
    #' @param collection_name Name of the collection.
    #'
    #' @return API response list describing optimizer state.
    #'
    #' @examples
    #' \dontrun{
    #'   collections$get_collection_optimizations("my_collection")
    #' }
    get_collection_optimizations = function(collection_name) {
      stopifnot(is.character(collection_name), nzchar(collection_name))
      url <- paste0(self$client$get_base_url(), "/collections/", collection_name, "/optimizations")
      self$client$make_request("GET", url)
    }
  )
)
