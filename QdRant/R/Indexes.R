
#' Indexes
#'
#' @description
#' Create and delete payload field indexes in a Qdrant collection. Indexes
#' speed up filtered searches on specific payload fields.
#'
#' @field client A \code{QdrantClient} instance.
#'
#' @export
Indexes <- R6::R6Class("Indexes",
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
    #' Create a payload field index to accelerate filtered searches.
    #'
    #' @param collection_name Name of the collection.
    #' @param field_name Payload field to index.
    #' @param field_schema (Optional) Character or named list specifying the
    #'   field type / schema. Supported types: \code{"keyword"},
    #'   \code{"integer"}, \code{"float"}, \code{"geo"}, \code{"text"},
    #'   \code{"bool"}, \code{"datetime"}, \code{"uuid"}. For text fields pass
    #'   a list with \code{type = "text"} plus optional tokenizer settings.
    #' @param ordering (Optional) Character. Write ordering guarantee.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   indexes <- Indexes$new(client)
    #'   indexes$create_payload_index("my_collection",
    #'     field_name   = "color",
    #'     field_schema = "keyword")
    #'
    #'   # Text index with tokenizer
    #'   indexes$create_payload_index("my_collection",
    #'     field_name   = "description",
    #'     field_schema = list(type = "text", tokenizer = "word"))
    #' }
    create_payload_index = function(collection_name, field_name,
                                    field_schema = NULL, ordering = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.character(field_name), nzchar(field_name))
      url  <- paste0(self$client$get_base_url(),
                     "/collections/", collection_name, "/index")
      body <- list(field_name = field_name)
      if (!is.null(field_schema)) body$field_schema <- field_schema
      query <- if (!is.null(ordering)) list(ordering = ordering) else NULL
      self$client$make_request("PUT", url, body, query = query)
    },

    #' @description
    #' Delete a payload field index.
    #'
    #' @param collection_name Name of the collection.
    #' @param field_name Name of the indexed field to remove.
    #' @param ordering (Optional) Character. Write ordering guarantee.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   indexes$delete_payload_index("my_collection", "color")
    #' }
    delete_payload_index = function(collection_name, field_name,
                                    ordering = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.character(field_name), nzchar(field_name))
      url   <- paste0(self$client$get_base_url(),
                      "/collections/", collection_name, "/index/", field_name)
      query <- if (!is.null(ordering)) list(ordering = ordering) else NULL
      self$client$make_request("DELETE", url, query = query)
    }
  )
)
