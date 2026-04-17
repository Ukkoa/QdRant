
#' Aliases
#'
#' @description
#' Manage collection aliases — atomic rename/swap operations that let you
#' point a stable name at different underlying collections.
#'
#' @field client A \code{QdrantClient} instance.
#'
#' @export
Aliases <- R6::R6Class("Aliases",
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
    #' Apply a batch of alias actions atomically.
    #'
    #' Each element of \code{actions} must be a named list with exactly one of:
    #' \describe{
    #'   \item{\code{create_alias}}{List with \code{collection_name} and \code{alias_name}.}
    #'   \item{\code{rename_alias}}{List with \code{old_alias_name} and \code{new_alias_name}.}
    #'   \item{\code{delete_alias}}{List with \code{alias_name}.}
    #' }
    #'
    #' @param actions List of alias action objects.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   aliases <- Aliases$new(client)
    #'
    #'   # Create an alias
    #'   aliases$update_aliases(actions = list(
    #'     list(create_alias = list(
    #'       collection_name = "my_collection_v2",
    #'       alias_name      = "my_collection"
    #'     ))
    #'   ))
    #'
    #'   # Rename an alias
    #'   aliases$update_aliases(actions = list(
    #'     list(rename_alias = list(
    #'       old_alias_name = "my_collection",
    #'       new_alias_name = "my_collection_old"
    #'     ))
    #'   ))
    #'
    #'   # Delete an alias
    #'   aliases$update_aliases(actions = list(
    #'     list(delete_alias = list(alias_name = "my_collection_old"))
    #'   ))
    #' }
    update_aliases = function(actions) {
      stopifnot(is.list(actions), length(actions) > 0)
      url  <- paste0(self$client$get_base_url(), "/collections/aliases")
      body <- list(actions = actions)
      self$client$make_request("POST", url, body)
    },

    #' @description
    #' List all aliases across all collections.
    #'
    #' @return API response list of alias objects.
    #'
    #' @examples
    #' \dontrun{
    #'   aliases$list_all_aliases()
    #' }
    list_all_aliases = function() {
      url <- paste0(self$client$get_base_url(), "/aliases")
      self$client$make_request("GET", url)
    },

    #' @description
    #' List all aliases pointing to a specific collection.
    #'
    #' @param collection_name Name of the collection.
    #'
    #' @return API response list of alias objects.
    #'
    #' @examples
    #' \dontrun{
    #'   aliases$list_collection_aliases("my_collection")
    #' }
    list_collection_aliases = function(collection_name) {
      stopifnot(is.character(collection_name), nzchar(collection_name))
      url <- paste0(self$client$get_base_url(),
                    "/collections/", collection_name, "/aliases")
      self$client$make_request("GET", url)
    },

    # -------------------------------------------------------------------------
    # Convenience wrappers
    # -------------------------------------------------------------------------

    #' @description
    #' Create a single alias (convenience wrapper around \code{update_aliases}).
    #'
    #' @param collection_name Name of the collection to alias.
    #' @param alias_name Alias name to create.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   aliases$create_alias("my_collection_v2", "my_collection")
    #' }
    create_alias = function(collection_name, alias_name) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.character(alias_name), nzchar(alias_name))
      self$update_aliases(actions = list(
        list(create_alias = list(collection_name = collection_name,
                                 alias_name = alias_name))
      ))
    },

    #' @description
    #' Rename an alias (convenience wrapper around \code{update_aliases}).
    #'
    #' @param old_alias_name Current alias name.
    #' @param new_alias_name New alias name.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   aliases$rename_alias("my_collection_old", "my_collection_archived")
    #' }
    rename_alias = function(old_alias_name, new_alias_name) {
      stopifnot(is.character(old_alias_name), nzchar(old_alias_name),
                is.character(new_alias_name), nzchar(new_alias_name))
      self$update_aliases(actions = list(
        list(rename_alias = list(old_alias_name = old_alias_name,
                                 new_alias_name = new_alias_name))
      ))
    },

    #' @description
    #' Delete a single alias (convenience wrapper around \code{update_aliases}).
    #'
    #' @param alias_name Alias name to delete.
    #'
    #' @return API response list.
    #'
    #' @examples
    #' \dontrun{
    #'   aliases$delete_alias("my_collection")
    #' }
    delete_alias = function(alias_name) {
      stopifnot(is.character(alias_name), nzchar(alias_name))
      self$update_aliases(actions = list(
        list(delete_alias = list(alias_name = alias_name))
      ))
    }
  )
)
