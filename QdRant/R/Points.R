
#' Points
#'
#' @description
#' Insert, update, delete, and query points (vectors + payloads) within a
#' Qdrant collection.
#'
#' @field client A \code{QdrantClient} instance.
#'
#' @export
Points <- R6::R6Class("Points",
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

    # -------------------------------------------------------------------------
    # Read
    # -------------------------------------------------------------------------

    #' @description
    #' Retrieve a single point by ID.
    #'
    #' @param collection_name Name of the collection.
    #' @param id Point ID (integer or UUID string).
    #' @param with_payload Logical or field selector. Return payload (default TRUE).
    #' @param with_vector Logical or vector names. Return vector(s) (default FALSE).
    #'
    #' @return API response list containing the point.
    #'
    #' @examples
    #' \dontrun{
    #'   points$retrieve_point("my_collection", 1)
    #' }
    retrieve_point = function(collection_name, id,
                              with_payload = TRUE, with_vector = FALSE) {
      stopifnot(is.character(collection_name), nzchar(collection_name))
      url   <- paste0(private$.base_url(),
                      "/collections/", collection_name, "/points/", id)
      query <- list(with_payload = with_payload, with_vector = with_vector)
      private$.req("GET", url, query = query)
    },

    #' @description
    #' Retrieve multiple points by ID.
    #'
    #' @param collection_name Name of the collection.
    #' @param ids Integer or character vector of point IDs.
    #' @param with_payload Logical or field selector (default TRUE).
    #' @param with_vector Logical or vector names (default FALSE).
    #' @param shard_key (Optional) Shard key to target a specific shard.
    #'
    #' @return API response list containing the points.
    #'
    #' @examples
    #' \dontrun{
    #'   points$retrieve_points("my_collection", ids = c(1, 2, 3))
    #' }
    retrieve_points = function(collection_name, ids,
                               with_payload = TRUE, with_vector = FALSE,
                               shard_key = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name))
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points")
      body <- list(ids = as.list(ids), with_payload = with_payload, with_vector = with_vector)
      if (!is.null(shard_key)) body$shard_key <- shard_key
      private$.req("POST", url, body)
    },

    #' @description
    #' Scroll through all points in a collection, optionally filtered.
    #'
    #' @param collection_name Name of the collection.
    #' @param limit Maximum number of points to return (default 10).
    #' @param offset Point ID to start from (for pagination).
    #' @param filter (Optional) Named list filter expression.
    #' @param with_payload Logical or field selector (default TRUE).
    #' @param with_vector Logical or vector names (default FALSE).
    #' @param order_by (Optional) Named list specifying ordering.
    #' @param shard_key (Optional) Shard key to target a specific shard.
    #'
    #' @return API response list with \code{points} and \code{next_page_offset}.
    #'
    #' @examples
    #' \dontrun{
    #'   points$scroll_points("my_collection", limit = 100)
    #' }
    scroll_points = function(collection_name, limit = 10, offset = NULL,
                             filter = NULL, with_payload = TRUE,
                             with_vector = FALSE, order_by = NULL,
                             shard_key = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.numeric(limit), limit > 0)
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/scroll")
      body <- list(limit = limit, with_payload = with_payload,
                   with_vector = with_vector)
      if (!is.null(offset))    body$offset    <- offset
      if (!is.null(filter))    body$filter    <- filter
      if (!is.null(order_by))  body$order_by  <- order_by
      if (!is.null(shard_key)) body$shard_key <- shard_key
      private$.req("POST", url, body)
    },

    #' @description
    #' Count points in a collection, optionally matching a filter.
    #'
    #' @param collection_name Name of the collection.
    #' @param filter (Optional) Named list filter expression.
    #' @param exact Logical. Use exact count (default TRUE; FALSE uses approximate).
    #'
    #' @return API response list with a \code{count} integer.
    #'
    #' @examples
    #' \dontrun{
    #'   points$count_points("my_collection")
    #' }
    count_points = function(collection_name, filter = NULL, exact = TRUE) {
      stopifnot(is.character(collection_name), nzchar(collection_name))
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/count")
      body <- list(exact = exact)
      if (!is.null(filter)) body$filter <- filter
      private$.req("POST", url, body)
    },

    #' @description
    #' Get value counts for a payload field (faceted statistics).
    #'
    #' @param collection_name Name of the collection.
    #' @param key Payload field name to facet on.
    #' @param limit Maximum number of facet values to return (default 10).
    #' @param filter (Optional) Named list filter expression to pre-filter points.
    #'
    #' @return API response list with facet hits.
    #'
    #' @examples
    #' \dontrun{
    #'   points$payload_field_facets("my_collection", key = "category")
    #' }
    payload_field_facets = function(collection_name, key, limit = 10,
                                    filter = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.character(key), nzchar(key))
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/facets")
      body <- list(key = key, limit = limit)
      if (!is.null(filter)) body$filter <- filter
      private$.req("POST", url, body)
    },

    # -------------------------------------------------------------------------
    # Write — points
    # -------------------------------------------------------------------------

    #' @description
    #' Upsert (insert or update) points into a collection.
    #'
    #' Each point requires an \code{id}; \code{vectors} and \code{payloads} are
    #' parallel lists aligned to \code{ids}.
    #'
    #' @param collection_name Name of the collection.
    #' @param ids Integer or UUID vector of point IDs.
    #' @param vectors (Optional) List of numeric vectors, one per ID.
    #' @param payloads (Optional) List of named lists, one per ID.
    #' @param ordering (Optional) Character. Write ordering guarantee.
    #' @param wait Logical. Wait for operation to complete (default TRUE).
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   points$upsert_points(
    #'     "my_collection",
    #'     ids      = c(1, 2),
    #'     vectors  = list(c(0.1, 0.2, 0.3), c(0.4, 0.5, 0.6)),
    #'     payloads = list(list(color = "red"), list(color = "blue"))
    #'   )
    #' }
    upsert_points = function(collection_name, ids, vectors = NULL,
                             payloads = NULL, ordering = NULL, wait = TRUE) {
      stopifnot(is.character(collection_name), nzchar(collection_name))
      if (is.null(ids) || length(ids) == 0) {
        stop("ids must be provided and cannot be empty.")
      }
      if (is.null(vectors) && is.null(payloads)) {
        stop("At least one of vectors or payloads must be provided.")
      }
      if (!is.null(vectors) && length(vectors) != length(ids)) {
        stop("Length of vectors must match length of ids.")
      }
      if (!is.null(payloads) && length(payloads) != length(ids)) {
        stop("Length of payloads must match length of ids.")
      }

      pts <- lapply(seq_along(ids), function(i) {
        pt <- list(id = ids[[i]])
        if (!is.null(vectors))  pt$vector  <- vectors[[i]]
        if (!is.null(payloads)) pt$payload <- payloads[[i]]
        pt
      })

      url   <- paste0(private$.base_url(),
                      "/collections/", collection_name, "/points")
      body  <- list(points = pts)
      query <- list(wait = tolower(as.character(wait)))
      if (!is.null(ordering)) query$ordering <- ordering
      private$.req("PUT", url, body, query = query)
    },

    #' @description
    #' Delete points by ID.
    #'
    #' @param collection_name Name of the collection.
    #' @param ids Integer or UUID vector of point IDs to delete.
    #' @param ordering (Optional) Character. Write ordering guarantee.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   points$delete_points_by_id("my_collection", ids = c(1, 2, 3))
    #' }
    delete_points_by_id = function(collection_name, ids, ordering = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name))
      if (is.null(ids) || length(ids) == 0) {
        stop("ids must be provided and cannot be empty.")
      }
      url   <- paste0(private$.base_url(),
                      "/collections/", collection_name, "/points/delete")
      body  <- list(points = as.list(ids))
      query <- if (!is.null(ordering)) list(ordering = ordering) else NULL
      private$.req("POST", url, body, query = query)
    },

    #' @description
    #' Delete points matching a filter expression.
    #'
    #' @param collection_name Name of the collection.
    #' @param filter Named list filter expression. All matching points are deleted.
    #' @param ordering (Optional) Character. Write ordering guarantee.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   points$delete_points_by_filter(
    #'     "my_collection",
    #'     filter = list(must = list(list(key = "color",
    #'                                   match = list(value = "red"))))
    #'   )
    #' }
    delete_points_by_filter = function(collection_name, filter,
                                       ordering = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.list(filter))
      url   <- paste0(private$.base_url(),
                      "/collections/", collection_name, "/points/delete")
      body  <- list(filter = filter)
      query <- if (!is.null(ordering)) list(ordering = ordering) else NULL
      private$.req("POST", url, body, query = query)
    },

    # -------------------------------------------------------------------------
    # Write — vectors
    # -------------------------------------------------------------------------

    #' @description
    #' Update vectors on existing points (partial update — other vectors unchanged).
    #'
    #' @param collection_name Name of the collection.
    #' @param points List of point objects, each with \code{id} and \code{vector}.
    #' @param ordering (Optional) Character. Write ordering guarantee.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   points$update_vectors(
    #'     "my_collection",
    #'     points = list(list(id = 1, vector = c(0.1, 0.2, 0.3)))
    #'   )
    #' }
    update_vectors = function(collection_name, points, ordering = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.list(points), length(points) > 0)
      url   <- paste0(private$.base_url(),
                      "/collections/", collection_name, "/points/vectors")
      body  <- list(points = points)
      query <- if (!is.null(ordering)) list(ordering = ordering) else NULL
      private$.req("PUT", url, body, query = query)
    },

    #' @description
    #' Delete named vectors from specific points (leaves other vectors intact).
    #'
    #' @param collection_name Name of the collection.
    #' @param ids Integer or UUID vector of point IDs.
    #' @param vector_names Character vector of vector names to delete.
    #' @param ordering (Optional) Character. Write ordering guarantee.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   points$delete_vectors("my_collection",
    #'     ids = c(1, 2), vector_names = c("image", "text"))
    #' }
    delete_vectors = function(collection_name, ids, vector_names,
                              ordering = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                length(ids) > 0, is.character(vector_names))
      url   <- paste0(private$.base_url(),
                      "/collections/", collection_name, "/points/vectors")
      body  <- list(points = as.list(ids), vector = vector_names)
      query <- if (!is.null(ordering)) list(ordering = ordering) else NULL
      private$.req("DELETE", url, body, query = query)
    },

    # -------------------------------------------------------------------------
    # Write — payload
    # -------------------------------------------------------------------------

    #' @description
    #' Merge payload fields into points (existing fields not in \code{payload}
    #' are kept).
    #'
    #' @param collection_name Name of the collection.
    #' @param payload Named list of payload fields to set.
    #' @param ids (Optional) Integer or UUID vector of target point IDs.
    #' @param filter (Optional) Filter selecting target points. One of \code{ids}
    #'   or \code{filter} must be provided.
    #' @param ordering (Optional) Character. Write ordering guarantee.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   points$set_payload("my_collection",
    #'     payload = list(color = "green"), ids = c(1, 2))
    #' }
    set_payload = function(collection_name, payload, ids = NULL,
                           filter = NULL, ordering = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.list(payload))
      if (is.null(ids) && is.null(filter)) {
        stop("One of ids or filter must be provided.")
      }
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/payload")
      body <- list(payload = payload)
      if (!is.null(ids))    body$points <- as.list(ids)
      if (!is.null(filter)) body$filter <- filter
      query <- if (!is.null(ordering)) list(ordering = ordering) else NULL
      private$.req("POST", url, body, query = query)
    },

    #' @description
    #' Replace the entire payload on points (all existing fields are removed).
    #'
    #' @param collection_name Name of the collection.
    #' @param payload Named list. New payload (replaces any existing payload).
    #' @param ids (Optional) Integer or UUID vector of target point IDs.
    #' @param filter (Optional) Filter selecting target points. One of \code{ids}
    #'   or \code{filter} must be provided.
    #' @param ordering (Optional) Character. Write ordering guarantee.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   points$overwrite_payload("my_collection",
    #'     payload = list(color = "green"), ids = c(1, 2))
    #' }
    overwrite_payload = function(collection_name, payload, ids = NULL,
                                 filter = NULL, ordering = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.list(payload))
      if (is.null(ids) && is.null(filter)) {
        stop("One of ids or filter must be provided.")
      }
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/payload")
      body <- list(payload = payload)
      if (!is.null(ids))    body$points <- as.list(ids)
      if (!is.null(filter)) body$filter <- filter
      query <- if (!is.null(ordering)) list(ordering = ordering) else NULL
      private$.req("PUT", url, body, query = query)
    },

    #' @description
    #' Delete specific payload fields from points.
    #'
    #' @param collection_name Name of the collection.
    #' @param keys Character vector of payload field names to remove.
    #' @param ids (Optional) Integer or UUID vector of target point IDs.
    #' @param filter (Optional) Filter selecting target points. One of \code{ids}
    #'   or \code{filter} must be provided.
    #' @param ordering (Optional) Character. Write ordering guarantee.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   points$delete_payload("my_collection", keys = "color", ids = c(1, 2))
    #' }
    delete_payload = function(collection_name, keys, ids = NULL,
                              filter = NULL, ordering = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.character(keys), length(keys) > 0)
      if (is.null(ids) && is.null(filter)) {
        stop("One of ids or filter must be provided.")
      }
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/payload/delete")
      body <- list(keys = as.list(keys))
      if (!is.null(ids))    body$points <- as.list(ids)
      if (!is.null(filter)) body$filter <- filter
      query <- if (!is.null(ordering)) list(ordering = ordering) else NULL
      private$.req("POST", url, body, query = query)
    },

    #' @description
    #' Remove all payload fields from points.
    #'
    #' @param collection_name Name of the collection.
    #' @param ids (Optional) Integer or UUID vector of target point IDs.
    #' @param filter (Optional) Filter selecting target points. One of \code{ids}
    #'   or \code{filter} must be provided.
    #' @param ordering (Optional) Character. Write ordering guarantee.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   points$clear_payload("my_collection", ids = c(1, 2))
    #' }
    clear_payload = function(collection_name, ids = NULL, filter = NULL,
                             ordering = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name))
      if (is.null(ids) && is.null(filter)) {
        stop("One of ids or filter must be provided.")
      }
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/payload/clear")
      body <- list()
      if (!is.null(ids))    body$points <- as.list(ids)
      if (!is.null(filter)) body$filter <- filter
      query <- if (!is.null(ordering)) list(ordering = ordering) else NULL
      private$.req("POST", url, body, query = query)
    },

    # -------------------------------------------------------------------------
    # Batch
    # -------------------------------------------------------------------------

    #' @description
    #' Execute multiple point operations in a single request.
    #'
    #' Each element of \code{operations} is a named list with a single key
    #' naming the operation type (e.g. \code{"upsert"}, \code{"delete"},
    #' \code{"set_payload"}) and a value containing the operation parameters.
    #'
    #' @param collection_name Name of the collection.
    #' @param operations List of operation objects.
    #' @param ordering (Optional) Character. Write ordering guarantee.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   points$batch_update_points("my_collection", operations = list(
    #'     list(upsert = list(points = list(list(id = 1, vector = c(0.1, 0.2)))))
    #'   ))
    #' }
    batch_update_points = function(collection_name, operations,
                                   ordering = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.list(operations), length(operations) > 0)
      url   <- paste0(private$.base_url(),
                      "/collections/", collection_name, "/points/batch")
      body  <- list(operations = operations)
      query <- if (!is.null(ordering)) list(ordering = ordering) else NULL
      private$.req("POST", url, body, query = query)
    }
  )
)
