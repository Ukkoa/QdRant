
#' VectorManager Class
#'
#' This class provides methods to manage vectors within a Qdrant collection. It allows
#' inserting vectors, searching vectors by similarity, and retrieving specific vectors by ID.
#'
#' @docType class
#' @format An R6 class object.
#' @field client An instance of the \code{QdrantClient} class used to make API requests.
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(client)}}{Creates a new VectorManager object with the given Qdrant client.}
#'   \item{\code{insert_vectors(collection_name, vectors_list)}}{Inserts a list of vectors into the specified collection. The input should be pre-formatted as a JSON list.}
#'   \item{\code{search_vectors(collection_name, vector, limit = 1, filter = NULL)}}{Searches for similar vectors in the collection based on the input vector.
#'   Returns the top \code{limit} results, optionally filtered by conditions.
#'   The \code{filter} argument can be used to specify conditions for filtering the search results, which should be provided in a JSON-like list format.
#'   For example, to filter results based on attributes, use:
#'   \code{filter = list(must = list(list(attribute_name = "value")),
#'                          should = list(list(attribute_name = "another_value")),
#'                          contains = list(list(attribute_name = "substring")))}.}
#'   \item{\code{get_vector(collection_name, id)}}{Retrieves a specific vector by its ID from the given collection.}
#' }
#' @examples
#' # Assuming `client` is an instance of QdrantClient
#' vector_manager <- VectorManager$new(client)
#'
#' # Insert vectors into a collection
#' vectors_list <- list(points = list(
#'   list(id = 1, vector = c(0.1, 0.2, 0.3), attributes = list(attribute_name = "value")),
#'   list(id = 2, vector = c(0.4, 0.5, 0.6), attributes = list(attribute_name = "another_value")),
#'   list(id = 3, vector = c(0.7, 0.8, 0.9), attributes = list(attribute_name = "substring"))
#' ))
#' vector_manager$insert_vectors("example_collection", vectors_list)
#'
#' # Search for similar vectors with a complex filter
#' search_results_filtered <- vector_manager$search_vectors("example_collection",
#'   c(0.1, 0.2, 0.3),
#'   limit = 5,
#'   filter = list(
#'     must = list(
#'       list(attribute_name = "value")  # Must have this exact attribute
#'     ),
#'     should = list(
#'       list(attribute_name = "another_value")  # Should match this attribute if possible
#'     ),
#'     contains = list(
#'       list(attribute_name = "sub")  # Contains this substring in the attribute
#'     )
#'   )
#' )
#'
#' # Get a specific vector by ID
#' vector <- vector_manager$get_vector("example_collection", 1)
#' @export
VectorManager <- R6::R6Class("VectorManager",
                         public = list(
                           client = NULL,

                           initialize = function(client) {
                             self$client <- client
                           },
                           ## this one is the odd one out requiring a prepped json list.
                           insert_vectors = function(collection_name, vectors_list) {
                             url <- paste0(self$client$get_base_url(), "/collections/", collection_name, "/points")
                             body <- vectors_list
                             return(self$client$make_request("PUT", url, body))
                           },

                           search_vectors = function(collection_name, vector, limit = 1, filter = NULL) {
                             url <- paste0(self$client$get_base_url(), "/collections/", collection_name, "/points/search")
                             body <- list(
                               vector = vector,
                               limit = limit,
                               filter = filter
                             )
                             return(self$client$make_request("POST", url, body))
                           },

                           get_vector = function(collection_name, id){

                             url <- paste0(self$client$get_base_url(), '/collections/', collection_name, '/points/', id)
                             return(self$client$make_request("GET", url))

                           }
                         )
)
