
#' CollectionManager Class
#'
#' This class manages collections in the Qdrant database. It provides methods
#' to create, retrieve details, list, check existence, and delete collections.
#'
#' @docType class
#' @format An R6 class object.
#' @field client An instance of the \code{QdrantClient} class used to make API requests.
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(client)}}{Creates a new CollectionManager object with the given Qdrant client.}
#'   \item{\code{create(collection_name, vector_size)}}{Creates a new collection with the specified name and vector size.}
#'   \item{\code{detail(collection_name)}}{Retrieves the details of a specific collection by its name.}
#'   \item{\code{list()}}{Lists all available collections.}
#'   \item{\code{exists(collection_name)}}{Checks if a specific collection exists.}
#'   \item{\code{delete(collection_name)}}{Deletes the specified collection by name.}
#' }
#' @examples
#' # Assuming `client` is an instance of QdrantClient
#' manager <- CollectionManager$new(client)
#'
#' # Create a collection
#' manager$create("example_collection", 128)
#'
#' # Get details of a collection
#' details <- manager$detail("example_collection")
#'
#' # List all collections
#' collections <- manager$list()
#'
#' # Check if a collection exists
#' exists <- manager$exists("example_collection")
#'
#' # Delete a collection
#' manager$delete("example_collection")
#' @export
CollectionManager <- R6::R6Class("CollectionManager",
                             public = list(
                               client = NULL,

                               initialize = function(client) {
                                 self$client <- client
                               },

                               create = function(collection_name, vector_size) {
                                 url <- paste0(self$client$get_base_url(), "/collections/", collection_name)
                                 body <- list(vectors = list(size = vector_size, distance = "Cosine"))
                                 return(self$client$make_request("PUT", url, body))
                               },

                               detail = function(collection_name){
                                 url <- paste0(self$client$get_base_url(), "/collections/", collection_name)
                                 return(self$client$make_request("GET", url))
                               },

                               list = function(){
                                 url <- paste0(self$client$get_base_url(), "/collections")
                                 return(self$client$make_request("GET", url))
                               },

                               exists = function(collection_name){

                                 url <- paste0(self$client$get_base_url(), "/collections/", collection_name,'/exists')
                                 return(self$client$make_request("GET", url))

                               },

                               delete = function(collection_name) {
                                 url <- paste0(self$client$get_base_url(), "/collections/", collection_name)
                                 return(self$client$make_request("DELETE", url))
                               }
                             )
)
