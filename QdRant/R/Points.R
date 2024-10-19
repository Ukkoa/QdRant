
#' @export
Points <- R6::R6Class("Points",
                         public = list(
                           client = NULL,

                           initialize = function(client) {
                             self$client <- client
                           },

                           retrieve_point = function(collection_name, id){

                             url <- paste0(self$client$get_base_url(), '/collections/', collection_name, '/points/', id)
                             return(self$client$make_request("GET", url))

                           },

                           retrieve_points = function(collection_name, ids){

                             url <- paste0(self$client$get_base_url(), '/collections/', collection_name, '/points', id)
                             body <- list(ids = ids)
                             return(self$client$make_request("POST", url))

                           },

                           upsert_points = function(collection_name, ids, vectors = NULL, payloads = NULL) {

                             if (is.null(ids) || length(ids) == 0) {
                               stop("Ids must be provided and cannot be NULL.")
                             }

                             if (is.null(vectors) && is.null(payloads)) {
                               stop("Either vectors or payloads must be provided. Both cannot be NULL.")
                             }

                             if (!is.null(vectors) && length(vectors) != length(ids)) {
                               stop("Length of vectors must match the length of ids or be NULL.")
                             }

                             if (!is.null(payloads) && length(payloads) != length(ids)) {
                               stop("Length of payloads must match the length of ids or be NULL.")
                             }

                             points <-
                               purrr::map(seq_along(ids), function(i) {
                                 point <- list(id = ids[i])
                                 if (!is.null(vectors)) {point$vector <- vectors[[i]]}
                                 if (!is.null(payloads)) {point$payload <- payloads[[i]]}
                                 return(point)
                             })

                             url  <- paste0(self$client$get_base_url(), '/collections/', collection_name, '/points')
                             body <- list(
                               collection_name = collection_name,
                               points = points
                             )

                             response <- self$client$make_request("PUT", url, body = body)

                             return(response)
                           }

                           ,

                           delete_points_by_id = function(collection_name, ids){
                            url <- paste0(self$client$get_base_url(), '/collections/', collection_name, '/points/delete', id)
                            body <- list(points = ids)
                            return(self$client$make_request("POST", url))

                           },

                           delete_points_by_filter = function(collection_name, filter){



                           },

                           update_vectors = function(){},
                           delete_vectors = function(){},
                           set_payload = function(){},
                           overwrite_payload = function(){},
                           delete_payload = function(){},
                           clear_payload = function(){},
                           batch_update_points = function(){},
                           scroll_points = function(){},
                           count_points = function(){},
                           payload_field_facets = function(){}

                         )
)
