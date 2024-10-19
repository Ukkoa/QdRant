
#' Collections
#'
#' @description
#' This class provides methods to manage collections in Qdrant.
#' It allows for creating, retrieving, listing, checking existence, and deleting collections.
#'
#' @field client A `QdrantClient` object to handle the connection and API requests.
#'
#' @export
Collections <- R6::R6Class("Collections",
                             public = list(
                               client = NULL,

                               #' @description
                               #' Initialize the Collections object with a Qdrant client.
                               #'
                               #' @param client An instance of the `QdrantClient` class to interact with the Qdrant API.
                               initialize = function(client) {
                                 self$client <- client
                               },

                               #' @description
                               #' Create a new collection in Qdrant.
                               #'
                               #' @param collection_name A character string representing the name of the collection to create.
                               #' @param vector_size An integer specifying the size of vectors stored in the collection.
                               #'
                               #' @return The response from the API, indicating whether the collection was created.
                               #' @examples
                               #' \dontrun{
                               #'   client <- QdrantClient$new()
                               #'   collections <- Collections$new(client)
                               #'   collections$create_collection("my_collection", 128)
                               #' }
                               create_collection = function(collection_name, vector_size) {
                                 url <- paste0(self$client$get_base_url(), "/collections/", collection_name)
                                 body <- list(vectors = list(size = vector_size, distance = "Cosine"))
                                 return(self$client$make_request("PUT", url, body))
                               },

                               #' @description
                               #' Get details of a specific collection.
                               #'
                               #' @param collection_name A character string representing the name of the collection.
                               #'
                               #' @return A list containing the details of the specified collection.
                               #' @examples
                               #' \dontrun{
                               #'   collections$get_collection_details("my_collection")
                               #' }
                               get_collection_details = function(collection_name){
                                 url <- paste0(self$client$get_base_url(), "/collections/", collection_name)
                                 return(self$client$make_request("GET", url))
                               },

                               #' @description
                               #' List all collections in the Qdrant instance.
                               #'
                               #' @return A list of collections stored in Qdrant.
                               #' @examples
                               #' \dontrun{
                               #'   collections$list_all_collections()
                               #' }
                               list_all_collections = function(){
                                 url <- paste0(self$client$get_base_url(), "/collections")
                                 return(self$client$make_request("GET", url))
                               },

                               #' @description
                               #' Check if a specific collection exists in Qdrant.
                               #'
                               #' @param collection_name A character string representing the name of the collection to check.
                               #'
                               #' @return A logical value indicating whether the collection exists (TRUE/FALSE).
                               #' @examples
                               #' \dontrun{
                               #'   collections$check_collection_existence("my_collection")
                               #' }
                               check_collection_existence = function(collection_name){

                                 url <- paste0(self$client$get_base_url(), "/collections/", collection_name,'/exists')
                                 return(self$client$make_request("GET", url))

                               },

                               #' @description
                               #' Delete a collection from Qdrant.
                               #'
                               #' @param collection_name A character string representing the name of the collection to delete.
                               #'
                               #' @return The response from the API indicating whether the collection was successfully deleted.
                               #' @examples
                               #' \dontrun{
                               #'   collections$delete_collection("my_collection")
                               #' }
                               delete_collection = function(collection_name) {
                                 url <- paste0(self$client$get_base_url(), "/collections/", collection_name)
                                 return(self$client$make_request("DELETE", url))
                               }
                             )
)
